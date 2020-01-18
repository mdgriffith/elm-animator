module Internal.Timeline exposing
    ( Timeline(..), TimelineDetails, Occurring(..), getEvents
    , Interpolator
    , Schedule(..), Event(..)
    , foldp, update, needsUpdate
    , startTime, endTime, getEvent, extendEventDwell, hasDwell
    , addToDwell
    , current, startPass, pass
    , Phase(..), Adjustment, Line(..), Timetable(..)
    , Description(..), Previous(..), atTime, gc, gcLog, linesAreActive, previousEndTime, updateNoGC
    )

{-|

@docs Timeline, TimelineDetails, Occurring, getEvents

@docs Interpolator

@docs Schedule, Event

@docs foldp, update, needsUpdate

@docs startTime, endTime, getEvent, extendEventDwell, hasDwell

@docs addToDwell

@docs current, startPass, pass

@docs Phase, Adjustment, Line, Timetable

-}

import Duration
import Internal.Time as Time
import Quantity
import Time


{-| A list of events that haven't been added to the schedule yet.
-}
type Schedule event
    = Schedule Time.Duration (Event event) (List (Event event))


{-| -}
type Event event
    = Event Time.Duration event (Maybe Time.Duration)


getScheduledEvent (Event _ ev _) =
    ev


adjustScheduledDuration fn (Event dur ev maybeDwell) =
    Event (fn dur) ev maybeDwell


{-| -}
type Timeline event
    = Timeline (TimelineDetails event)


type alias TimelineDetails event =
    { initial : event
    , now : Time.Absolute
    , events : Timetable event
    , queued : Maybe (Schedule event)
    , interruption : List (Schedule event)
    , running : Bool
    }


{-| A time table is a list of timelines that will occur.

Events proceed from earlier to later.

Lines are ordered earliest to latest.

-}
type Timetable event
    = Timetable (List (Line event))


{-| -}
type Line event
    = Line Time.Absolute (Occurring event) (List (Occurring event))


{-| When the event occurs, and how long we're dwelling at this event (or no dwelling at all)
-}
type Occurring event
    = Occurring event Time.Absolute (Maybe Time.Duration)



{- TYPES FOR INTERPOLATION -}


type Phase
    = Start
    | Transitioning


type alias Starter event anchor state =
    (event -> anchor)
    -> Occurring event
    -> state


type alias Interpolator event anchor state =
    (event -> anchor)
    -> Previous event
    -> Occurring event
    -> Maybe (Occurring event)
    -> Phase
    -> Time.Absolute
    -> state
    -> state


type Previous event
    = Previous (Occurring event)
    | PreviouslyInterrupted Time.Absolute


type alias Adjustment =
    { arrivingEarly : Float
    , leavingLate : Float
    }


type alias TimeAdjustor anchor =
    anchor -> Adjustment


mapTable : (Occurring a -> Occurring b) -> Timetable a -> Timetable b
mapTable fn (Timetable lines) =
    Timetable (List.map (mapLine fn) lines)


mapLine : (Occurring a -> Occurring b) -> Line a -> Line b
mapLine fn (Line t startEvent els) =
    Line t (fn startEvent) (List.map fn els)


mapTableWith : (Occurring a -> state -> ( Occurring b, state )) -> state -> Timetable a -> ( Timetable b, state )
mapTableWith fn initial (Timetable lines) =
    let
        overLines line ( existingLines, state ) =
            let
                ( newLine, newLineState ) =
                    mapLineWith fn state line
            in
            ( newLine :: existingLines, newLineState )

        ( reversedUpdatedLines, finalState ) =
            List.foldl overLines ( [], initial ) lines
    in
    ( Timetable (List.reverse reversedUpdatedLines)
    , finalState
    )


mapLineWith : (Occurring a -> state -> ( Occurring b, state )) -> state -> Line a -> ( Line b, state )
mapLineWith fn initial (Line start startingEvent remaining) =
    let
        onLine occur ( events, state ) =
            let
                ( newOccur, newState ) =
                    fn occur state
            in
            ( newOccur :: events, newState )

        ( newStartingEvent, startingState ) =
            fn startingEvent initial
    in
    case List.foldl onLine ( [], startingState ) remaining of
        ( reversedEvents, newState ) ->
            ( Line start newStartingEvent (List.reverse reversedEvents), newState )


foldLine : (Occurring a -> b -> b) -> b -> Line a -> b
foldLine fn cursor (Line start startingEvent remaining) =
    List.foldl fn cursor (startingEvent :: remaining)


filterMapOccurring : (event -> Maybe newEvent) -> Occurring event -> Maybe (Occurring newEvent)
filterMapOccurring fn (Occurring ev time maybeDwell) =
    case fn ev of
        Nothing ->
            Nothing

        Just newEv ->
            Just (Occurring newEv time maybeDwell)


getEvent : Occurring event -> event
getEvent (Occurring ev _ _) =
    ev


extendEventDwell : Time.Duration -> Event event -> Event event
extendEventDwell extendBy ((Event at ev maybeDwell) as thisEvent) =
    if Duration.inMilliseconds extendBy == 0 then
        thisEvent

    else
        Event at ev (addToDwell extendBy maybeDwell)


hasDwell : Occurring event -> Bool
hasDwell (Occurring _ _ maybeDwell) =
    maybeDwell /= Nothing


startTime : Occurring event -> Time.Absolute
startTime (Occurring _ time maybeDwell) =
    time


endTime : Occurring event -> Time.Absolute
endTime (Occurring _ time maybeDwell) =
    case maybeDwell of
        Nothing ->
            time

        Just dwell ->
            Time.advanceBy dwell time


previousEndTime : Previous event -> Time.Absolute
previousEndTime prev =
    case prev of
        Previous event ->
            endTime event

        PreviouslyInterrupted time ->
            time


{-| -}
needsUpdate : Timeline event -> Bool
needsUpdate (Timeline timeline) =
    (timeline.queued /= Nothing)
        || timeline.running


type Description event
    = DescribeStartTransition Time.Posix
    | DescribeEvent Time.Posix event
    | DescribeInterruption
        { interruption : Time.Posix
        , target : event
        , newTarget : event
        , newTargetTime : Time.Posix
        }


getEvents : Timeline event -> List (List ( Time.Posix, event ))
getEvents (Timeline timeline) =
    case timeline.events of
        Timetable lines ->
            lines
                |> List.map (\(Line _ start ev) -> start :: ev)
                |> List.map (List.map (\(Occurring evt time maybeDwell) -> ( Time.toPosix time, evt )))


atTime : Time.Posix -> Timeline event -> Timeline event
atTime now (Timeline timeline) =
    Timeline { timeline | now = Time.absolute now }


{-| -}
update : Time.Posix -> Timeline event -> Timeline event
update possiblyNow (Timeline timeline) =
    let
        -- we can only move forward with updating
        -- This is so that the GC doesn't cause awkward skips.
        -- NOTE: for something like debug mode, we might want to disable this
        -- to allow scrubbing a timeline.
        now =
            Quantity.max (Time.absolute possiblyNow) timeline.now
    in
    if timeline.events == Timetable [] then
        { timeline
            | now = now
            , events =
                let
                    firstOccurring =
                        Occurring timeline.initial now Nothing
                in
                Timetable
                    [ Line now firstOccurring []
                    ]
        }
            |> applyQueued
            |> applyInterruptions
            |> clean True
            |> Timeline

    else
        { timeline | now = now }
            |> applyQueued
            |> applyInterruptions
            |> clean True
            |> Timeline


updateNoGC : Time.Posix -> Timeline event -> Timeline event
updateNoGC now (Timeline timeline) =
    if timeline.events == Timetable [] then
        { timeline
            | now = Time.absolute now
            , events =
                let
                    firstOccurring =
                        Occurring timeline.initial (Time.absolute now) Nothing
                in
                Timetable
                    [ Line (Time.absolute now) firstOccurring []
                    ]
        }
            |> applyQueued
            |> applyInterruptions
            |> clean False
            |> Timeline

    else
        { timeline | now = Time.absolute now }
            |> applyQueued
            |> applyInterruptions
            |> clean False
            |> Timeline


{-| Garbage collect and update `isRunning`
-}
clean : Bool -> TimelineDetails event -> TimelineDetails event
clean runGC details =
    let
        events =
            case details.events of
                Timetable evs ->
                    evs

        running =
            case details.events of
                Timetable lines ->
                    linesAreActive details.now lines
    in
    { details
        | running = running
        , events =
            if runGC then
                Timetable (garbageCollectOldEvents details.now [] events)

            else
                details.events
    }


gc : Timeline event -> Timeline event
gc (Timeline details) =
    let
        events =
            case details.events of
                Timetable evs ->
                    evs
    in
    Timeline { details | events = Timetable (garbageCollectOldEvents details.now [] events) }


gcLog : Timeline event -> Timeline event
gcLog (Timeline details) =
    let
        events =
            case details.events of
                Timetable evs ->
                    evs
    in
    Timeline { details | events = Timetable (garbageCollectOldEventsLog details.now [] events) }


{-| If we're dwelling at an event, we can reset the event we're dwelling on to the base of the timeline.

All previous lines can be dropped.

However, if we're not dwelling, we want to keep the previous lines.

So we track "droppable" lines until we meet a dwell.

-}
garbageCollectOldEvents : Time.Absolute -> List (Line event) -> List (Line event) -> List (Line event)
garbageCollectOldEvents now droppable lines =
    case lines of
        [] ->
            List.reverse droppable

        (Line startAt startingEvent events) :: remaining ->
            if Time.thisAfterOrEqualThat startAt now then
                -- this line hasn't happened yet
                List.reverse droppable ++ lines

            else if dwellingAt now startingEvent then
                -- we can safetly drop the droppables
                lines

            else
                let
                    maybeInterruptionTime =
                        remaining
                            |> List.head
                            |> Maybe.map lineStartTime

                    interrupted =
                        case maybeInterruptionTime of
                            Nothing ->
                                False

                            Just interruptionTime ->
                                Time.thisAfterOrEqualThat now interruptionTime
                in
                if interrupted then
                    -- reduce a
                    garbageCollectOldEvents now (Line startAt startingEvent events :: droppable) remaining

                else
                    case List.foldl (shortenLine now) NotFinished events of
                        NotFinished ->
                            List.reverse droppable ++ lines

                        AfterLine ->
                            case List.head remaining of
                                Nothing ->
                                    case List.head (List.reverse events) of
                                        Nothing ->
                                            lines

                                        Just last ->
                                            [ Line (startTime last) last [] ]

                                Just (Line startNext next _) ->
                                    if Time.thisAfterOrEqualThat now startNext then
                                        -- the next line has started
                                        -- this current line can be dropped if we're dwelling
                                        garbageCollectOldEvents now (Line startAt startingEvent events :: droppable) remaining

                                    else
                                        List.reverse droppable ++ lines

                        DwellingAt newLine ->
                            reverseEvents newLine :: remaining


{-| If we're dwelling at an event, we can reset the event we're dwelling on to the base of the timeline.

All previous lines can be dropped.

However, if we're not dwelling, we want to keep the previous lines.

So we track "droppable" lines until we meet a dwell.

-}
garbageCollectOldEventsLog : Time.Absolute -> List (Line event) -> List (Line event) -> List (Line event)
garbageCollectOldEventsLog now droppable lines =
    let
        _ =
            Debug.log "gc" "now"
    in
    case lines of
        [] ->
            List.reverse droppable

        (Line startAt startingEvent events) :: remaining ->
            if Debug.log "starting" <| Time.thisAfterOrEqualThat startAt now then
                -- this line hasn't happened yet
                List.map (Debug.log "lines") (List.reverse droppable ++ lines)

            else if Debug.log "dwelling at" <| dwellingAt now startingEvent then
                -- we can safetly drop the droppables
                lines
                -- List.reverse droppable ++ lines

            else
                let
                    maybeInterruptionTime =
                        remaining
                            |> List.head
                            |> Maybe.map lineStartTime

                    interrupted =
                        case maybeInterruptionTime of
                            Nothing ->
                                False

                            Just interruptionTime ->
                                Time.thisAfterOrEqualThat now interruptionTime
                in
                if Debug.log "interrupted" interrupted then
                    -- reduce a
                    garbageCollectOldEvents now (Line startAt startingEvent events :: droppable) remaining

                else
                    case Debug.log "shorten" <| List.foldl (shortenLine now) NotFinished events of
                        NotFinished ->
                            List.reverse droppable ++ lines

                        AfterLine ->
                            case List.head remaining of
                                Nothing ->
                                    case List.head (List.reverse events) of
                                        Nothing ->
                                            lines

                                        Just last ->
                                            [ Line (startTime last) last [] ]

                                Just (Line startNext next _) ->
                                    if Time.thisAfterOrEqualThat now startNext then
                                        -- the next line has started
                                        -- this current line can be dropped if we're dwelling
                                        garbageCollectOldEvents now (Line startAt startingEvent events :: droppable) remaining

                                    else
                                        List.reverse droppable ++ lines

                        -- List.reverse droppable ++ lines
                        DwellingAt newLine ->
                            -- newLine :: lines
                            reverseEvents newLine :: remaining


lineStartTime (Line start _ _) =
    start


reverseEvents (Line start event evs) =
    Line start event (List.reverse evs)


type LineStatus event
    = NotFinished
    | DwellingAt (Line event)
    | AfterLine


shortenLine now event dwellingAtEvent =
    case dwellingAtEvent of
        NotFinished ->
            if Time.thisAfterOrEqualThat now (endTime event) then
                AfterLine

            else if dwellingAt now event then
                DwellingAt (Line (startTime event) event [])

            else
                NotFinished

        DwellingAt (Line time start remaining) ->
            DwellingAt (Line time start (event :: remaining))

        AfterLine ->
            if dwellingAt now event then
                DwellingAt (Line (startTime event) event [])

            else if Time.thisAfterOrEqualThat now (startTime event) then
                AfterLine

            else
                NotFinished


dwellingAt : Time.Absolute -> Occurring event -> Bool
dwellingAt now event =
    let
        eventEndTime =
            endTime event

        eventStartTime =
            startTime event
    in
    Time.thisAfterOrEqualThat now eventStartTime
        && Time.thisBeforeOrEqualThat now eventEndTime


linesAreActive : Time.Absolute -> List (Line event) -> Bool
linesAreActive now lines =
    case lines of
        [] ->
            False

        (Line startAt startingEvent events) :: remaining ->
            if Time.thisAfterOrEqualThat startAt now then
                True

            else
                let
                    last =
                        List.reverse events
                            |> List.head
                            |> Maybe.withDefault startingEvent

                    maybeInterruption =
                        case List.head remaining of
                            Nothing ->
                                Nothing

                            Just (Line interruptionTime _ _) ->
                                Just interruptionTime
                in
                case maybeInterruption of
                    Just interruptTime ->
                        -- interuption hasn't happened yet, so we need to continue till it does
                        if Time.thisAfterOrEqualThat interruptTime now then
                            True

                        else
                            case last of
                                Occurring _ time _ ->
                                    if Time.thisAfterOrEqualThat time now then
                                        True

                                    else
                                        linesAreActive now remaining

                    Nothing ->
                        case last of
                            Occurring _ time _ ->
                                if Time.thisAfterOrEqualThat time now then
                                    True

                                else
                                    linesAreActive now remaining


applyQueued : TimelineDetails event -> TimelineDetails event
applyQueued timeline =
    case timeline.queued of
        Nothing ->
            timeline

        Just queued ->
            { timeline
                | events =
                    enqueue timeline timeline.now queued
                , queued = Nothing
            }


applyInterruptions : TimelineDetails event -> TimelineDetails event
applyInterruptions timeline =
    case timeline.interruption of
        [] ->
            timeline

        _ ->
            applyInterruptionHelper timeline.interruption { timeline | interruption = [] }


applyInterruptionHelper : List (Schedule event) -> TimelineDetails event -> TimelineDetails event
applyInterruptionHelper interrupts timeline =
    case interrupts of
        [] ->
            timeline

        inter :: remaining ->
            let
                delay =
                    case inter of
                        Schedule d _ _ ->
                            d

                newEvents =
                    interrupt timeline (Time.advanceBy delay timeline.now) inter
            in
            applyInterruptionHelper remaining { timeline | events = newEvents }


{-| Interrupt a current timetable with a new list of events.

    - If this timeline is after all other timelines
        -> queue it to the end and extend the dwell of the last event
    - otherwise, add as a new `Line` to the timetable.

-}
interrupt : TimelineDetails events -> Time.Absolute -> Schedule events -> Timetable events
interrupt details startAt scheduled =
    case details.events of
        Timetable lines ->
            case interruptLines startAt scheduled [] lines of
                Nothing ->
                    enqueue details startAt scheduled

                Just interrupted ->
                    Timetable interrupted


interruptLines : Time.Absolute -> Schedule event -> List (Line event) -> List (Line event) -> Maybe (List (Line event))
interruptLines startInterruption scheduled pastLines lines =
    case lines of
        [] ->
            Nothing

        startLine :: remaining ->
            if interruptionHappensLater startInterruption remaining then
                interruptLines startInterruption scheduled (startLine :: pastLines) remaining

            else
                case interruptLine startInterruption scheduled startLine remaining of
                    Nothing ->
                        interruptLines startInterruption scheduled (startLine :: pastLines) remaining

                    Just interruption ->
                        -- interruption is the interruption in the proper order, embedded with remaining
                        if startInterruption == lineStartTime startLine then
                            -- if the times are the same, then this new line replaces the current one.
                            Just (List.reverse pastLines ++ interruption)

                        else
                            Just (List.reverse pastLines ++ (startLine :: interruption))


interruptionHappensLater startInterruption remaining =
    case remaining of
        [] ->
            False

        top :: _ ->
            Time.thisAfterOrEqualThat startInterruption (lineStartTime top)


interruptLine startInterruption scheduled line future =
    case line of
        Line start startEvent trailing ->
            if Time.thisAfterOrEqualThat startInterruption start then
                -- this line starts before the interruption
                case future of
                    [] ->
                        case getTransitionAt startInterruption startEvent trailing of
                            Nothing ->
                                Just
                                    [ createLine startInterruption scheduled
                                    ]

                            Just last2Events ->
                                Just
                                    [ interruptAtExactly startInterruption scheduled last2Events ]

                    (Line nextStart next nextEvents) :: futureRemaining ->
                        -- we need to find the target event we're currently enroute to.
                        -- if the next line has already started, but the event hasnt happened yet
                        -- then we know `next` is the target
                        if Time.thisAfterOrEqualThat startInterruption nextStart && Time.thisBeforeOrEqualThat startInterruption (startTime next) then
                            Just
                                (Line nextStart next nextEvents
                                    :: interruptAtExactly startInterruption
                                        scheduled
                                        (LastTwoEvents (endTime startEvent) (getEvent startEvent) (startTime next) (getEvent next))
                                    :: futureRemaining
                                )

                        else
                            Nothing

            else
                Nothing


getTransitionAt interruptionTime startEvent trailing =
    case trailing of
        [] ->
            Nothing

        next :: remain ->
            let
                previous =
                    List.head remain
                        |> Maybe.withDefault startEvent
            in
            if Time.thisAfterOrEqualThat interruptionTime (endTime previous) && Time.thisBeforeThat interruptionTime (startTime next) then
                Just (LastTwoEvents (endTime previous) (getEvent previous) (startTime next) (getEvent next))

            else
                getTransitionAt interruptionTime startEvent remain


interruptAtExactly startInterruption scheduled (LastTwoEvents penultimateTime penultimate lastEventTime lastEvent) =
    case scheduled of
        Schedule delay_ startingEvent reverseQueued ->
            let
                progress =
                    Time.progress penultimateTime lastEventTime startInterruption

                newStartingEvent =
                    if penultimate == getScheduledEvent startingEvent then
                        startingEvent
                            |> adjustScheduledDuration (Quantity.multiplyBy progress)

                    else
                        startingEvent
            in
            createLine startInterruption
                (Schedule delay_ newStartingEvent reverseQueued)


type LastTwoEvents event
    = LastTwoEvents Time.Absolute event Time.Absolute event


findInterruptedTransition startInterruption events =
    case events of
        _ ->
            Nothing


getLastEventTime : List (Line event) -> Maybe Time.Absolute
getLastEventTime lines =
    case List.head (List.reverse lines) of
        Nothing ->
            Nothing

        Just (Line start startEvent trailing) ->
            case List.reverse trailing of
                [] ->
                    Just (startTime startEvent)

                (Occurring _ at maybeDwell) :: _ ->
                    Just at


{-| Queue a list of events to be played after everything.

    - add events to the timeline that is currently active.
    - if we're past all events,
        -> add additional dwell time to the last event.

-}
enqueue : TimelineDetails events -> Time.Absolute -> Schedule events -> Timetable events
enqueue timeline now scheduled =
    case timeline.events of
        Timetable lines ->
            Timetable (addToCurrentLine now scheduled lines)


addToCurrentLine : Time.Absolute -> Schedule event -> List (Line event) -> List (Line event)
addToCurrentLine now scheduled lines =
    let
        onCurrent timelines =
            case timelines of
                [] ->
                    [ createLine now scheduled ]

                (Line startOne startEvent one) :: [] ->
                    -- if we've gotten here, this line is current
                    [ addEventsToLine now scheduled (Line startOne startEvent one) ]

                (Line startOne startEventOne one) :: (Line startTwo startEventTwo two) :: remaining ->
                    -- we check if now is after startOne, but before startTwo
                    if Time.thisAfterOrEqualThat now startOne && Time.thisAfterOrEqualThat startTwo now then
                        -- one is the current timeline
                        addEventsToLine now scheduled (Line startOne startEventOne one)
                            :: Line startTwo startEventTwo two
                            :: remaining

                    else
                        -- need to search farther.
                        Line startOne startEventOne one
                            :: onCurrent (Line startTwo startEventTwo two :: remaining)
    in
    onCurrent lines


createLine : Time.Absolute -> Schedule events -> Line events
createLine now (Schedule _ (Event dur startEvent maybeDwell) reverseQueued) =
    let
        start =
            Time.advanceBy dur now

        startNextEvent =
            case maybeDwell of
                Nothing ->
                    start

                Just dwell ->
                    Time.advanceBy dwell start

        events =
            List.foldl toOccurring ( startNextEvent, [] ) (List.reverse reverseQueued)
                |> Tuple.second
                |> List.reverse
    in
    Line now (Occurring startEvent start maybeDwell) events


addEventsToLine : Time.Absolute -> Schedule events -> Line events -> Line events
addEventsToLine now (Schedule delay scheduledStartingEvent reverseQueued) (Line startLineAt startingEvent events) =
    let
        queued =
            scheduledStartingEvent :: List.reverse reverseQueued

        reversedEvents =
            List.reverse events

        start =
            Time.advanceBy delay now
    in
    case reversedEvents of
        [] ->
            let
                startNewEventsAt =
                    Time.latest (startTime startingEvent) start

                startingEventWithDwell =
                    case startingEvent of
                        Occurring ev lastEventTime maybeDwell ->
                            if Time.thisAfterThat start lastEventTime then
                                Occurring ev lastEventTime (Just (Time.duration start lastEventTime))

                            else
                                Occurring ev lastEventTime Nothing
            in
            List.foldl toOccurring ( startNewEventsAt, [] ) queued
                |> Tuple.second
                |> List.reverse
                |> Line startLineAt startingEventWithDwell

        (Occurring lastEvent lastEventTime finalEventDwell) :: eventTail ->
            let
                startNewEventsAt =
                    Time.latest lastEventTime start

                newEvents =
                    List.foldl toOccurring ( startNewEventsAt, [] ) queued
                        |> Tuple.second
                        |> List.reverse
            in
            if Time.thisAfterOrEqualThat start lastEventTime then
                -- if the start of the scheduled events is after the last event time,
                -- then we need to increase the dwell time of the last event to match the start time.
                let
                    newLastEvent =
                        Occurring lastEvent
                            lastEventTime
                            (if Time.thisAfterThat start lastEventTime then
                                Just (Time.duration start lastEventTime)

                             else
                                Nothing
                            )
                in
                Line startLineAt
                    startingEvent
                    (List.reverse (newLastEvent :: eventTail)
                        ++ newEvents
                    )

            else
                Line startLineAt startingEvent (events ++ newEvents)


toOccurring : Event event -> ( Time.Absolute, List (Occurring event) ) -> ( Time.Absolute, List (Occurring event) )
toOccurring (Event duration event maybeDwell) ( now, events ) =
    let
        occursAt =
            Time.advanceBy duration now

        endsAt =
            case maybeDwell of
                Nothing ->
                    occursAt

                Just dwell ->
                    Time.advanceBy dwell occursAt
    in
    ( endsAt, Occurring event occursAt maybeDwell :: events )


extendDwell : Time.Duration -> Occurring a -> Occurring a
extendDwell newDwell ((Occurring at ev maybeDwell) as occur) =
    if Duration.inMilliseconds newDwell == 0 then
        occur

    else
        Occurring at ev (addToDwell newDwell maybeDwell)


addToDwell : Time.Duration -> Maybe Time.Duration -> Maybe Time.Duration
addToDwell duration maybeDwell =
    if Duration.inMilliseconds duration == 0 then
        maybeDwell

    else
        case maybeDwell of
            Nothing ->
                Just duration

            Just existing ->
                Just (Quantity.plus duration existing)


getOccurringTime (Occurring _ t _) =
    t


{-| This is a "Fold from the past", just like foldl is "fold from the left".

It's named as a nod to Elm's original foldp when `Signal`s were still a thing!
<https://package.elm-lang.org/packages/elm-lang/core/3.0.0/Signal>

However, this foldp is a bit different.

Ok, we have a few subtle concepts here.

1.  `event` is the event type provided by the user.
    _Example_ `ShowModal`

2.  `anchor` is the direct value that the event is mapped to.
    _Example_ {x,y} or {angle}
    This is essentially the communicated constraint that is desired.

3.  `motion` is the anchor plus additional data informed by previous and upcoming `anchors` as well as timestamps.
    _Example_ {x,y,velocity}

And we have some type aliases to capture how to create each one of those values.

    `Interpolator` is a function that maps linearly between two `motions`.

-}
foldp :
    (event -> anchor)
    -> Starter event anchor motion
    -> Maybe (TimeAdjustor anchor)
    -> Interpolator event anchor motion
    -> Timeline event
    -> motion
foldp lookup starter maybeAdjustTiming interpolate ((Timeline timelineDetails) as timeline) =
    case timelineDetails.events of
        Timetable timetable ->
            let
                maybeNextEvent =
                    Nothing

                startingEvent =
                    case List.head timetable of
                        Nothing ->
                            Occurring timelineDetails.initial timelineDetails.now Nothing

                        Just (Line _ start _) ->
                            start

                startingCursor =
                    starter lookup startingEvent
            in
            foldOverLines Beginning lookup maybeAdjustTiming interpolate timelineDetails startingCursor timetable Nothing


{-| There's some subtlty to starting the fold over timelines.

We always want a `previous event` to be available because it's important to a lot of calculations that involve adjusting the timeline last minute.

SO, how do we start everything?

-}
type Beginning
    = Beginning
    | Continuing


foldOverLines :
    Beginning
    -> (event -> anchor)
    -> Maybe (TimeAdjustor anchor)
    -> Interpolator event anchor motion
    -> TimelineDetails event
    -> motion
    -> List (Line event)
    -> Maybe (Cursor event motion)
    -> motion
foldOverLines beginning lookup maybeAdjustor interpolate timeline startingState lines existingCursor =
    case lines of
        [] ->
            -- Generally, this shouldn't be reachable because we require an event on initialization
            -- likely we'll want to change events to `(start, [remaining])` at some point in the future
            -- interpolate lookup (Occurring timeline.initial timeline.now Nothing) Nothing Start
            startingState

        (Line startAt startingEvent events) :: remaining ->
            let
                maybeInterruption =
                    case List.head remaining of
                        Nothing ->
                            Nothing

                        Just (Line interruptAt interruptEv interruptEvents) ->
                            Just interruptAt

                cursor =
                    List.foldl
                        (overEvents timeline.now maybeInterruption lookup maybeAdjustor interpolate)
                        (case existingCursor of
                            Nothing ->
                                { state = startingState
                                , events = startingEvent :: events
                                , previous =
                                    Previous startingEvent
                                , beginning = beginning
                                , status =
                                    NotDone
                                , previousAdjustment =
                                    case maybeAdjustor of
                                        Nothing ->
                                            Nothing

                                        Just adjustment ->
                                            -- Just
                                            --     { previousEventTime = startTime startingEvent
                                            --     , applied = adjustment
                                            --     }
                                            -- TODO
                                            Nothing
                                }

                            Just existing ->
                                { existing
                                    | events = startingEvent :: events
                                    , status =
                                        NotDone
                                }
                        )
                        (startingEvent :: events)
            in
            if cursor.status == Finished then
                cursor.state

            else
                foldOverLines cursor.beginning
                    lookup
                    maybeAdjustor
                    interpolate
                    timeline
                    cursor.state
                    remaining
                    (Just cursor)


type alias Cursor event state =
    { state : state
    , events : List (Occurring event)
    , previous : Previous event
    , status : Status
    , beginning : Beginning
    , previousAdjustment :
        Maybe
            { previousEventTime : Time.Absolute
            , applied : Adjustment
            }
    }


type Status
    = Finished
    | NotDone
    | Interrupted


overEvents :
    Time.Absolute
    -> Maybe Time.Absolute
    -> (event -> anchor)
    -> Maybe (TimeAdjustor anchor)
    -> Interpolator event anchor motion
    -> Occurring event
    -> Cursor event motion
    -> Cursor event motion
overEvents now maybeInterruption lookup maybeAdjustor interpolate _ cursor =
    case cursor.status of
        Finished ->
            cursor

        Interrupted ->
            cursor

        NotDone ->
            case cursor.events of
                [] ->
                    cursor

                target :: remaining ->
                    let
                        lookAhead =
                            List.head remaining

                        adjustedLookAhead =
                            Maybe.map applyLookAheadAdjustment lookAhead

                        applyLookAheadAdjustment ahead =
                            case maybeAdjustment of
                                Nothing ->
                                    ahead

                                Just targetAdjustment ->
                                    case maybeAdjustor of
                                        Nothing ->
                                            ahead

                                        Just adjustor ->
                                            Tuple.first
                                                (applyAdjustment lookup
                                                    adjustor
                                                    { previousEventTime = endTime target
                                                    , applied = targetAdjustment
                                                    }
                                                    ahead
                                                    Nothing
                                                )

                        ( adjustedTarget, maybeAdjustment ) =
                            case maybeAdjustor of
                                Nothing ->
                                    ( target, Nothing )

                                Just adjustor ->
                                    case cursor.previousAdjustment of
                                        Nothing ->
                                            ( target, Nothing )

                                        Just previousAdjustment ->
                                            applyAdjustment lookup adjustor previousAdjustment target lookAhead
                    in
                    case getPhase cursor.beginning adjustedTarget now maybeInterruption of
                        ( status, phase, currentTime ) ->
                            { state =
                                interpolate
                                    lookup
                                    cursor.previous
                                    adjustedTarget
                                    adjustedLookAhead
                                    phase
                                    currentTime
                                    cursor.state
                            , events = remaining
                            , previous =
                                -- This is preemptively setting the "previous" target
                                -- however, if there has been an interruption, our previous doesn't change
                                case status of
                                    Interrupted ->
                                        PreviouslyInterrupted currentTime

                                    _ ->
                                        Previous adjustedTarget
                            , status = status
                            , beginning = Continuing
                            , previousAdjustment =
                                case maybeAdjustment of
                                    Nothing ->
                                        Nothing

                                    Just adjustment ->
                                        Just
                                            { previousEventTime = endTime target
                                            , applied = adjustment
                                            }
                            }


zeroDuration : Duration.Duration
zeroDuration =
    Duration.milliseconds 0


{-|

    if target.arriveEarly then
        adjust time earlier
        extend dwell (add dwell if none)
    if target.leaveLate
        extend dwell even further

-}
applyAdjustment lookup adjustor { previousEventTime, applied } ((Occurring event time maybeDwell) as target) maybeLookAhead =
    let
        targetAdjustments =
            adjustor (lookup event)

        totalDuration =
            Time.duration previousEventTime time

        totalPortions =
            max
                (applied.leavingLate + targetAdjustments.arrivingEarly)
                1

        earlyBy =
            Quantity.multiplyBy
                (targetAdjustments.arrivingEarly / totalPortions)
                totalDuration

        lateBy =
            case maybeLookAhead of
                Nothing ->
                    zeroDuration

                Just lookAhead ->
                    let
                        lookAheadAdjustments =
                            adjustor (lookup (getEvent lookAhead))

                        totalLookAheadDuration =
                            Time.duration (endTime target) (startTime lookAhead)

                        totalLookAheadPortions =
                            max
                                (targetAdjustments.leavingLate + lookAheadAdjustments.arrivingEarly)
                                1
                    in
                    Quantity.multiplyBy
                        (targetAdjustments.leavingLate / totalLookAheadPortions)
                        totalLookAheadDuration
    in
    ( Occurring event (Time.rollbackBy earlyBy time) (addToDwell (Quantity.plus earlyBy lateBy) maybeDwell)
    , Just targetAdjustments
    )


{-|

    Params:
        1. Previous Event
        2. Current Event
        3. Now
        4. Maybe interruption time
        5. state

When we get an interruption, we want to skip all events.

-}
getPhase : Beginning -> Occurring event -> Time.Absolute -> Maybe Time.Absolute -> ( Status, Phase, Time.Absolute )
getPhase beginning target now maybeInterruption =
    case beginning of
        Beginning ->
            ( if dwellingAt now target then
                Finished

              else
                NotDone
            , Start
            , now
            )

        Continuing ->
            let
                interrupted =
                    case maybeInterruption of
                        Nothing ->
                            False

                        Just interruptTime ->
                            Time.thisBeforeOrEqualThat interruptTime (startTime target)
                                && Time.thisAfterThat now interruptTime
            in
            if interrupted then
                let
                    interruptionTime =
                        Maybe.withDefault now maybeInterruption
                in
                ( Interrupted
                , Transitioning
                , interruptionTime
                )

            else if Time.thisAfterThat now (endTime target) then
                ( NotDone, Transitioning, now )

            else
                ( Finished, Transitioning, now )



{- BOOKKEEPING -}
{- We need some standard way to cleanup a timeline so it doesn't grow out of control.

   What are some ways we could flag events to be removed.

       1. Don't flag them.
       2. If an event has occurred before, and occurs again, remove the older one if it's not involved in an interruption.
           - means iterating through the entire timeline, and keeping track of what happened before.

       3. Flag by depth.  timeline can only grow to x number of events.
           -- Essentially Just `List.take n timeline` on each timeline.
           -- Can be an issue if we're using `after`, or any sort of folding,
           -- where we're depending on an event that is chopped.

           -> `After` is important because
               -> What if we have a modal or a menu that opens
               -> Then inside of that menu, a bunch of stuff happens.
               -> The modal open state now needs to be reflected in each one of those tiny animations


               This seems sorta doable with a nested type.  Such as:
                   Animator.opacity model.timeline <|
                       \event ->
                           case event of
                               Modal _ ->
                                   1

                               _ ->
                                   0


               But falls apart when we try to implement those animations.  Consider modeling two checkboxes that are being checked.

                   Animator.opacity model.timeline <|
                       \event ->
                           case event of
                               Modal CheckedOne ->
                                   1

                               _ ->
                                   0


                   Animator.opacity model.timeline <|
                       \event ->
                           case event of
                               Modal CheckedTwo ->
                                   1

                               _ ->
                                   0


               Now, checking one box will "uncheck" the previous ones.  The problem is that we have one timeline, when we need parallel timelines.



               Using `After`:

                   Animator.opacity (Animator.after (Modal CheckedTwo) model.timeline) <|
                       \event ->
                           case event of
                               True ->
                                   1

                               False ->
                                   0


               Using `Between`:
                   Animator.opacity (Animator.between ModalOpen ModalClosed model.timeline) <|
                       \event ->
                           case event of
                               True ->
                                   1

                               False ->
                                   0


               However, instead!  we could just have a timeline of our states.

               So, we have

               `Animator.Timeline Bool` for our checkbox.






   # Synced states

   Instead of

       `Animator.rewrite : newEvent -> Timeline event -> (event -> Maybe newEvent) -> Timeline newEvent`

       Where the likely usage would be to recalculate an entire new timeline in the view.
       It also prevents us from having the flag-by-depth garbage collection, which could be done automatically.



-}


current : Timeline event -> Occurring event
current timeline =
    foldp identity
        startPass
        Nothing
        pass
        timeline


startPass : (event -> event) -> Occurring event -> Occurring event
startPass lookup start =
    start


pass : (event -> event) -> Previous event -> Occurring event -> Maybe (Occurring event) -> Phase -> Time.Absolute -> Occurring event -> Occurring event
pass _ _ target _ _ _ _ =
    target


currentAndPrevious : Timeline event -> ( Occurring event, Occurring event )
currentAndPrevious timeline =
    foldp identity
        startCurrentAndPrevious
        Nothing
        passCurrentAndPrevious
        timeline


startCurrentAndPrevious : (event -> event) -> Occurring event -> ( Occurring event, Occurring event )
startCurrentAndPrevious lookup start =
    ( start, start )


passCurrentAndPrevious : (event -> event) -> Previous event -> Occurring event -> Maybe (Occurring event) -> Phase -> Time.Absolute -> ( Occurring event, Occurring event ) -> ( Occurring event, Occurring event )
passCurrentAndPrevious _ _ target _ _ _ ( _, prevTarget ) =
    ( prevTarget, target )
