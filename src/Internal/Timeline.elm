module Internal.Timeline exposing
    ( Timeline(..), TimelineDetails, Occurring(..), getEvents
    , Schedule(..), Event(..)
    , update, needsUpdate
    , startTime, endTime, getEvent, extendEventDwell, hasDwell
    , addToDwell
    , progress, dwellingTime
    , current
    , Adjustment, Line(..), Timetable(..)
    , foldp, capture, captureTimeline
    , ActualDuration(..), Animator(..), Description(..), Frame(..), Frames(..), FramesSummary, Interp, LookAhead, Oscillator(..), Pause(..), Period(..), Previous(..), Resting(..), Summary, SummaryEvent(..), atTime, gc, hasChanged, justInitialized, linesAreActive, prepareOscillator, previous, previousEndTime, previousStartTime, updateNoGC
    )

{-|

@docs Timeline, TimelineDetails, Occurring, getEvents

@docs Schedule, Event

@docs update, needsUpdate

@docs startTime, endTime, getEvent, extendEventDwell, hasDwell

@docs addToDwell

@docs progress, dwellingTime

@docs current

@docs Adjustment, Line, Timetable

@docs foldp, capture, captureTimeline

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


type Period
    = Loop Time.Duration
    | Repeat Int Time.Duration


getScheduledEvent : Event event -> event
getScheduledEvent (Event _ ev _) =
    ev


adjustScheduledDuration : (Time.Duration -> Time.Duration) -> Event event -> Event event
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
type
    Line event
    --   maybe previous event, starting time, starting event, subsequent events
    -- The maybe previous event is only used to calculate time adjustments for arriveEarly and leaveLate
    = Line Time.Absolute (Occurring event) (List (Occurring event))


{-| When the event occurs and the end
-}
type Occurring event
    = Occurring event Time.Absolute Time.Absolute



{- TYPES FOR INTERPOLATION -}


type alias Interp state anchor motion =
    { start : anchor -> motion
    , adjustor : TimeAdjustor anchor
    , dwellFor : DwellFor anchor motion
    , dwellPeriod : DwellPeriod anchor
    , after : After state anchor motion
    , lerp : Lerp anchor motion
    }


type alias DwellFor anchor state =
    anchor -> Time.Duration -> state


type alias After event anchor state =
    (event -> anchor) -> Occurring event -> List (Occurring event) -> state


type alias Milliseconds =
    Float


type alias LookAhead movement =
    { anchor : movement
    , time : Milliseconds
    , resting : Bool
    }


type alias Lerp anchor motion =
    -- (event -> anchor) -> Previous event -> Occurring event -> List (Occurring event) -> Time.Absolute -> state -> state
    Milliseconds
    -> Maybe anchor
    -> anchor
    -> Milliseconds
    -> Milliseconds
    -> Maybe (LookAhead anchor)
    -> motion
    -> motion


type alias DwellPeriod anchor =
    anchor -> Maybe Period


type Previous event
    = Previous (Occurring event)
    | PreviouslyInterrupted Time.Absolute


{-| -}
type Oscillator
    = Oscillator (List Pause) (Float -> Float)
    | Resting Float


{-| -}
type Pause
    = Pause Time.Duration Float


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
hasDwell (Occurring _ (Quantity.Quantity start) (Quantity.Quantity end)) =
    (start - end) /= 0


{-| Start time is potentially earlier based on the previous event and `arriveEarly`.

If there is no previous event, this doesnt need to be called.

No need to create and pass in a Maybe.

-}
startTimeAdj : (event -> anchor) -> TimeAdjustor anchor -> Occurring event -> Occurring event -> Time.Absolute
startTimeAdj lookup getAdjustment (Occurring prev _ prevEnd) (Occurring cur curStartTime _) =
    let
        adjustment =
            getAdjustment (lookup cur)

        prevAdjustment =
            getAdjustment (lookup prev)

        totalDuration =
            Time.duration prevEnd curStartTime

        -- if portions sum to more than 1, then that sum represents the full duration
        totalPortions =
            max
                (prevAdjustment.leavingLate + adjustment.arrivingEarly)
                1

        earlyBy =
            Quantity.multiplyBy
                (adjustment.arrivingEarly / totalPortions)
                totalDuration
    in
    Time.rollbackBy earlyBy curStartTime


{-| End time is potentially later based on the next event and `leaveLate`.

If there is no next event, this doesnt need to be called.

No need to create and pass in a Maybe.

-}
endTimeAdj : (event -> anchor) -> TimeAdjustor anchor -> Occurring event -> Occurring event -> Time.Absolute
endTimeAdj lookup getAdjustment (Occurring cur _ curEnd) (Occurring next nextStartTime _) =
    let
        adjustment =
            getAdjustment (lookup cur)

        nextAdjustment =
            getAdjustment (lookup next)

        totalDuration =
            Time.duration curEnd nextStartTime

        -- if portions sum to more than 1, then that sum represents the full duration
        totalPortions =
            max
                (adjustment.leavingLate + nextAdjustment.arrivingEarly)
                1

        lateBy =
            Quantity.multiplyBy
                (adjustment.leavingLate / totalPortions)
                totalDuration
    in
    Time.advanceBy lateBy curEnd


startTime : Occurring event -> Time.Absolute
startTime (Occurring _ time _) =
    time


endTime : Occurring event -> Time.Absolute
endTime (Occurring _ _ end) =
    end


previousEndTime : Previous event -> Time.Absolute
previousEndTime prev =
    case prev of
        Previous event ->
            endTime event

        PreviouslyInterrupted time ->
            time


previousStartTime : Previous event -> Time.Absolute
previousStartTime prev =
    case prev of
        Previous event ->
            startTime event

        PreviouslyInterrupted time ->
            time


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
                |> List.map (List.map (\(Occurring evt time _) -> ( Time.toPosix time, evt )))


atTime : Time.Posix -> Timeline event -> Timeline event
atTime now (Timeline timeline) =
    Timeline { timeline | now = Time.absolute now }


{-| -}
update : Time.Posix -> Timeline event -> Timeline event
update possiblyNow (Timeline timeline) =
    let
        -- we can only move forward with updating
        -- This is so that the Animator event "GC" doesn't cause awkward skips.
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
                        Occurring timeline.initial now now
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
    let
        absoluteNow =
            Time.absolute now
    in
    if timeline.events == Timetable [] then
        { timeline
            | now = absoluteNow
            , events =
                let
                    firstOccurring =
                        Occurring timeline.initial absoluteNow absoluteNow
                in
                Timetable
                    [ Line absoluteNow firstOccurring []
                    ]
        }
            |> applyQueued
            |> applyInterruptions
            |> clean False
            |> Timeline

    else
        { timeline | now = absoluteNow }
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


log str x =
    x


{-| If we're dwelling at an event, we can reset the event we're dwelling on to the base of the timeline.

All previous lines can be dropped.

However, if we're not dwelling, we want to keep the previous lines.

So we track "droppable" lines until we meet a dwell.

-}
garbageCollectOldEvents : Time.Absolute -> List (Line event) -> List (Line event) -> List (Line event)
garbageCollectOldEvents now droppable lines =
    log " <-----" <|
        case lines of
            [] ->
                List.reverse droppable

            (Line startAt startingEvent events) :: remaining ->
                if log "hasnt happened yet" <| Time.thisAfterOrEqualThat startAt now then
                    -- this line hasn't happened yet
                    List.reverse droppable ++ lines

                else if log "dwelling at" <| dwellingAt now startingEvent then
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
                    if log "interrupted" <| interrupted then
                        garbageCollectOldEvents now (Line startAt startingEvent events :: droppable) remaining

                    else
                        -- case List.foldl (shortenLine now) NotFinished events of
                        --     NotFinished ->
                        --         List.reverse droppable ++ lines
                        --     AfterLine ->
                        --         case List.head remaining of
                        --             Nothing ->
                        --                 case List.head (List.reverse events) of
                        --                     Nothing ->
                        --                         lines
                        --                     Just last ->
                        --                         [ Line (startTime last) last [] ]
                        --             Just (Line startNext next _) ->
                        --                 if Time.thisAfterOrEqualThat now startNext then
                        --                     -- the next line has started
                        --                     -- this current line can be dropped if we're dwelling
                        --                     garbageCollectOldEvents now (Line startAt startingEvent events :: droppable) remaining
                        --                 else
                        --                     List.reverse droppable ++ lines
                        --     DwellingAt newLine ->
                        --         reverseEvents newLine :: remaining
                        case log "hewed" <| hewLine now events of
                            NothingCaptured ->
                                -- case Debug.log "remaining" <| List.head remaining of
                                --     Nothing ->
                                --         case List.head (List.reverse events) of
                                --             Nothing ->
                                --                 List.reverse droppable ++ lines
                                --             Just last ->
                                --                 [ Line (startTime last) last [] ]
                                --     Just (Line startNext next _) ->
                                --         if Time.thisAfterOrEqualThat now startNext then
                                --             -- the next line has started
                                --             -- this current line can be dropped if we're dwelling
                                --             garbageCollectOldEvents now (Line startAt startingEvent events :: droppable) remaining
                                --         else
                                --             List.reverse droppable ++ lines
                                List.reverse droppable ++ lines

                            Captured capturedLine ->
                                capturedLine :: remaining


reverseEvents (Line start event evs) =
    Line start event (List.reverse evs)


type HewStatus event
    = Captured (Line event)
    | NothingCaptured


hewLine now events =
    case events of
        [] ->
            NothingCaptured

        top :: remaining ->
            if dwellingAt now top then
                Captured (Line (startTime top) top remaining)

            else if Time.thisAfterThat now (endTime top) then
                hewLine now remaining

            else
                NothingCaptured


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


lineStartTime (Line start _ _) =
    start


beforeLineEnd time (Line lineStartAt startingEvent trailing) =
    if Time.thisBeforeOrEqualThat time lineStartAt then
        True

    else
        case trailing of
            [] ->
                Time.thisBeforeThat time (endTime startingEvent)

            _ ->
                beforeEventEnd time trailing


beforeEventEnd time events =
    case events of
        [] ->
            False

        top :: remain ->
            if Time.thisBeforeThat time (endTime top) then
                True

            else
                beforeEventEnd time remain


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
            case interruptLines details.now startAt scheduled [] lines of
                Nothing ->
                    enqueue details startAt scheduled

                Just interrupted ->
                    Timetable interrupted


interruptLines : Time.Absolute -> Time.Absolute -> Schedule event -> List (Line event) -> List (Line event) -> Maybe (List (Line event))
interruptLines now startInterruption scheduled pastLines lines =
    case lines of
        [] ->
            Nothing

        startLine :: remaining ->
            if interruptionHappensLater startInterruption remaining then
                interruptLines now startInterruption scheduled (startLine :: pastLines) remaining

            else
                case interruptLine startInterruption scheduled startLine remaining of
                    Nothing ->
                        interruptLines now startInterruption scheduled (startLine :: pastLines) remaining

                    Just interruption ->
                        -- if the starting times are the same
                        -- then this new line replaces the current one.
                        if startInterruption == lineStartTime startLine && Time.thisAfterThat startInterruption now then
                            Just (List.reverse pastLines ++ interruption)

                        else
                            -- interruption is the interruption in the proper order, embedded with remaining
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
                                if beforeLineEnd startInterruption line then
                                    Just
                                        [ createLine startInterruption scheduled
                                        ]

                                else
                                    -- we'll just queue up this new line instead
                                    Nothing

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
                prev =
                    List.head remain
                        |> Maybe.withDefault startEvent
            in
            if Time.thisAfterOrEqualThat interruptionTime (endTime prev) && Time.thisBeforeThat interruptionTime (startTime next) then
                Just (LastTwoEvents (endTime prev) (getEvent prev) (startTime next) (getEvent next))

            else
                getTransitionAt interruptionTime startEvent remain


interruptAtExactly startInterruption scheduled (LastTwoEvents penultimateTime penultimate lastEventTime lastEvent) =
    case scheduled of
        Schedule delay_ startingEvent reverseQueued ->
            let
                amountProgress =
                    Time.progress penultimateTime lastEventTime startInterruption

                newStartingEvent =
                    if penultimate == getScheduledEvent startingEvent then
                        startingEvent
                            |> adjustScheduledDuration (Quantity.multiplyBy amountProgress)

                    else
                        startingEvent
            in
            createLine startInterruption
                (Schedule delay_ newStartingEvent reverseQueued)


type LastTwoEvents event
    = LastTwoEvents Time.Absolute event Time.Absolute event


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
    Line now (Occurring startEvent start startNextEvent) events


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
                        Occurring ev lastEventTime _ ->
                            if Time.thisAfterThat start lastEventTime then
                                Occurring ev lastEventTime start

                            else
                                Occurring ev lastEventTime lastEventTime
            in
            List.foldl toOccurring ( startNewEventsAt, [] ) queued
                |> Tuple.second
                |> List.reverse
                |> Line startLineAt startingEventWithDwell

        (Occurring lastEvent lastEventTime _) :: eventTail ->
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
                                start

                             else
                                lastEventTime
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
    case events of
        [] ->
            ( endsAt, [ Occurring event occursAt endsAt ] )

        prev :: remain ->
            if startTime prev == occursAt then
                -- then this event would supercede the previous one
                ( endsAt, Occurring event occursAt endsAt :: remain )

            else
                ( endsAt, Occurring event occursAt endsAt :: events )


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


{-| This is a "Fold from the past", just like foldl is "fold from the left".

It's named as a nod to Elm's original foldp when `Signal`s were still a thing!
<https://package.elm-lang.org/packages/elm-lang/core/3.0.0/Signal>

However, this foldp is a bit different.

Ok, we have a few subtle concepts here.

1.  `state` is the state type provided by the user.
    _Example_ `ShowModal`

2.  `anchor` is the direct value that the event is mapped to.
    _Example_ {x,y} or {angle}
    This is essentially the communicated constraint that is desired.

3.  `motion` is the anchor plus additional data informed by previous and upcoming `anchors` as well as timestamps.
    _Example_ {x,y,velocity}

And we have some type aliases to capture how to create each one of those values.

    `Interpolator` is a function that maps linearly between two `motions`.

There are also two modes:

Normally we return a value directly at `now`.

In the case where we want to render things ahead of time, we want to do that but THEN
we want to proceed at regular time intervals until the timeline is finished.

In that case we also want to capture the dwell state at the end of the timeline in some way.

-}
foldp :
    (state -> anchor)
    -> Interp state anchor motion
    -> Timeline state
    -> motion
foldp lookup fn (Timeline timelineDetails) =
    case timelineDetails.events of
        Timetable timetable ->
            let
                start =
                    fn.start (lookup timelineDetails.initial)
            in
            case timetable of
                [] ->
                    start

                firstLine :: remainingLines ->
                    overLines
                        fn
                        lookup
                        timelineDetails
                        Nothing
                        firstLine
                        remainingLines
                        start


overLines :
    Interp state anchor motion
    -> (state -> anchor)
    -> TimelineDetails state
    -- current line
    -> Maybe (Occurring state)
    -> Line state
    -> List (Line state)
    -> motion
    -> motion
overLines fn lookup details maybePreviousEvent (Line lineStart lineStartEv lineRemain) futureLines state =
    -- futureStart starts a new line.
    -- if an interruption occurs, we want to interpolate to the point of the interruption
    -- then transition over to the new line.
    let
        transition newState =
            -- After we interpolate, we check here if we are actually done
            -- or if we were just interrupted and need to keep going
            case futureLines of
                [] ->
                    newState

                ((Line futureStart futureStartEv futureRemain) as future) :: restOfFuture ->
                    if Time.thisBeforeOrEqualThat futureStart details.now then
                        -- NOTE, we could pass an event to maybePreviousEvent
                        -- which might make time adjustments accurate across transitions
                        overLines fn lookup details Nothing future restOfFuture newState

                    else
                        newState

        now =
            case futureLines of
                [] ->
                    details.now

                (Line futureStart futureStartEv futureRemain) :: restOfFuture ->
                    if Time.thisBeforeThat futureStart details.now then
                        futureStart

                    else
                        details.now

        eventStartTime =
            case maybePreviousEvent of
                Nothing ->
                    startTime lineStartEv

                Just prev ->
                    startTimeAdj lookup fn.adjustor prev lineStartEv
    in
    if Time.thisBeforeThat now eventStartTime then
        -- lerp from state to lineStartEv
        -- now is before the first event start time.
        -- either this is an interruption, in which case we lerp.
        -- Or this is the
        -- if we're
        -- fn.lerp lookup
        --     (Previous (Occurring details.initial lineStart lineStart))
        --     lineStartEv
        --     lineRemain
        --     now
        --     state
        fn.lerp
            (Time.inMilliseconds lineStart)
            (Just (lookup details.initial))
            (lookup (getEvent lineStartEv))
            (Time.inMilliseconds eventStartTime)
            (Time.inMilliseconds now)
            (case lineRemain of
                [] ->
                    Nothing

                upcoming :: _ ->
                    Just
                        { anchor = lookup (getEvent upcoming)
                        , time = Time.inMilliseconds (startTimeAdj lookup fn.adjustor lineStartEv upcoming)
                        , resting =
                            not (hasDwell upcoming)
                        }
            )
            state
            |> transition

    else
        let
            eventEndTime =
                case lineRemain of
                    [] ->
                        endTime lineStartEv

                    upcoming :: _ ->
                        endTimeAdj lookup fn.adjustor lineStartEv upcoming
        in
        if Time.thisAfterOrEqualThat now eventEndTime then
            -- after linestartEv
            case lineRemain of
                [] ->
                    -- dwell at lineStartEv, there's nothing to transition to
                    fn.dwellFor
                        (lookup (getEvent lineStartEv))
                        (Time.duration eventStartTime now)
                        |> transition

                next :: lineRemain2 ->
                    let
                        nextStartTime =
                            startTimeAdj lookup fn.adjustor lineStartEv next

                        nextEndTime =
                            case lineRemain2 of
                                [] ->
                                    endTime next

                                upcoming :: _ ->
                                    endTimeAdj lookup fn.adjustor next upcoming
                    in
                    if Time.thisBeforeThat now nextStartTime then
                        -- Before next.startTime
                        --     -> lerp start to next
                        -- fn.lerp
                        --     lookup
                        --     (Previous lineStartEv)
                        --     next
                        --     lineRemain2
                        --     now
                        -- (if hasDwell lineStartEv then
                        --     fn.dwellFor (lookup (getEvent lineStartEv)) (Time.duration eventStartTime eventEndTime)
                        --  else
                        --     state
                        -- )
                        fn.lerp
                            (Time.inMilliseconds eventEndTime)
                            (Just (lookup (getEvent lineStartEv)))
                            (lookup (getEvent next))
                            (Time.inMilliseconds nextStartTime)
                            (Time.inMilliseconds now)
                            (case lineRemain2 of
                                [] ->
                                    Nothing

                                upcoming :: _ ->
                                    Just
                                        { anchor = lookup (getEvent upcoming)
                                        , time = Time.inMilliseconds (startTimeAdj lookup fn.adjustor next upcoming)
                                        , resting =
                                            not (hasDwell upcoming)
                                        }
                            )
                            (if hasDwell lineStartEv then
                                fn.dwellFor (lookup (getEvent lineStartEv)) (Time.duration eventStartTime eventEndTime)

                             else
                                state
                            )
                            |> transition

                    else if Time.thisBeforeThat now nextEndTime then
                        -- After next.startTime
                        --- Before next.endTime
                        --      -> we're dwelling at `next`
                        fn.dwellFor
                            (lookup (getEvent next))
                            (Time.duration nextStartTime now)
                            |> transition

                    else
                        -- After lineStart.endTime
                        -- After next.startTime
                        -- After next.endTime
                        case lineRemain2 of
                            [] ->
                                -- Nothing to continue on to,
                                --      -> we're dwelling at `next`
                                fn.dwellFor
                                    (lookup (getEvent next))
                                    (Time.duration nextStartTime now)
                                    |> transition

                            next2 :: lineRemain3 ->
                                -- continue on
                                let
                                    next2StartTime =
                                        startTimeAdj lookup fn.adjustor next next2

                                    next2EndTime =
                                        case lineRemain3 of
                                            [] ->
                                                endTime next2

                                            upcoming :: _ ->
                                                endTimeAdj lookup fn.adjustor next2 upcoming
                                in
                                if Time.thisBeforeThat now next2StartTime then
                                    let
                                        after =
                                            if hasDwell next then
                                                fn.dwellFor
                                                    (lookup (getEvent next))
                                                    (Time.duration nextStartTime nextEndTime)

                                            else
                                                fn.after lookup next lineRemain2
                                    in
                                    -- fn.lerp
                                    --     lookup
                                    --     (Previous next)
                                    --     next2
                                    --     lineRemain3
                                    --     now
                                    --     after
                                    --     |> transition
                                    fn.lerp
                                        -- next end time?
                                        (Time.inMilliseconds nextEndTime)
                                        (Just (lookup (getEvent next)))
                                        (lookup (getEvent next2))
                                        (Time.inMilliseconds next2StartTime)
                                        (Time.inMilliseconds now)
                                        (case lineRemain3 of
                                            [] ->
                                                Nothing

                                            upcoming :: _ ->
                                                Just
                                                    { anchor = lookup (getEvent upcoming)
                                                    , time = Time.inMilliseconds (startTimeAdj lookup fn.adjustor next2 upcoming)
                                                    , resting =
                                                        not (hasDwell upcoming)
                                                    }
                                        )
                                        after
                                        |> transition

                                else if Time.thisBeforeThat now next2EndTime then
                                    -- we're dwelling at `next2`
                                    fn.dwellFor
                                        (lookup (getEvent next2))
                                        (Time.duration next2StartTime now)
                                        |> transition

                                else
                                    let
                                        after =
                                            if hasDwell next2 then
                                                fn.dwellFor (lookup (getEvent next2))
                                                    (Time.duration next2StartTime next2EndTime)

                                            else
                                                fn.after lookup next2 lineRemain3
                                    in
                                    -- Recompose our line by removing previous events
                                    -- and continue forward!
                                    overLines
                                        fn
                                        lookup
                                        details
                                        (Just next)
                                        (Line nextEndTime next2 lineRemain3)
                                        futureLines
                                        after

        else
            -- dwell at linestartEv
            -- we've checked that it's not after or before,
            -- so it has to be between the start and end
            fn.dwellFor (lookup (getEvent lineStartEv))
                (Time.duration eventStartTime now)
                |> transition


type alias FramesSummary motion =
    { frames : List (Frame motion)
    , duration : Time.Duration
    , dwell :
        Maybe
            { period : Period
            , frames : List (Frame motion)
            }
    }


type Frame motion
    = Frame Float motion


type alias FramesPerSecond =
    Float


capture :
    FramesPerSecond
    -> (state -> anchor)
    -> Interp state anchor motion
    -> Timeline state
    -> FramesSummary motion
capture fps lookup fn (Timeline timelineDetails) =
    case timelineDetails.events of
        Timetable timetable ->
            let
                start =
                    fn.start (lookup timelineDetails.initial)
            in
            case timetable of
                [] ->
                    { frames = [ Frame 1 start ]
                    , duration = zeroDuration
                    , dwell = Nothing
                    }

                firstLine :: remainingLines ->
                    let
                        lastEvent =
                            findLastEventInLines firstLine remainingLines

                        ( _, numberOfFrames ) =
                            if Time.thisAfterThat timelineDetails.now (startTime lastEvent) then
                                ( 0, 1 )

                            else
                                Time.numberOfFrames fps
                                    timelineDetails.now
                                    timelineDetails.now
                                    (startTime lastEvent)

                        millisecondsPerFrame =
                            1000 / fps

                        frames =
                            getFrames (startTime lastEvent)
                                { perFrame =
                                    Duration.milliseconds millisecondsPerFrame
                                , offset = 0
                                , startTime = timelineDetails.now
                                , endTime = startTime lastEvent
                                }
                                (\currentTime ->
                                    overLines
                                        fn
                                        lookup
                                        -- maybe we break this out to a separate param to avoid updating?
                                        { timelineDetails | now = currentTime }
                                        Nothing
                                        firstLine
                                        remainingLines
                                        start
                                )
                                []
                    in
                    { frames = frames
                    , duration =
                        if Time.thisAfterThat timelineDetails.now (startTime lastEvent) then
                            zeroDuration

                        else
                            Time.duration timelineDetails.now (startTime lastEvent)
                    , dwell =
                        case fn.dwellPeriod (lookup (getEvent lastEvent)) of
                            Nothing ->
                                Nothing

                            Just period ->
                                let
                                    dwellStartTime =
                                        startTime lastEvent

                                    endAt =
                                        case period of
                                            Repeat n totalDur ->
                                                Time.advanceBy totalDur dwellStartTime

                                            Loop totalDur ->
                                                Time.advanceBy totalDur dwellStartTime

                                    lastEventEv =
                                        lookup (getEvent lastEvent)

                                    ( dwellOffset, numberOfDwellFrames ) =
                                        Time.numberOfFrames fps
                                            -- TODO: we might need some sort of correction
                                            -- frames.lastFrameTime
                                            dwellStartTime
                                            dwellStartTime
                                            endAt

                                    dwellFrames =
                                        getFrames endAt
                                            { perFrame = Duration.milliseconds millisecondsPerFrame
                                            , offset = dwellOffset
                                            , startTime = dwellStartTime
                                            , endTime = endAt
                                            }
                                            (\currentTime ->
                                                fn.dwellFor lastEventEv
                                                    (Time.duration dwellStartTime currentTime)
                                            )
                                            []
                                in
                                Just
                                    { period = period
                                    , frames = dwellFrames
                                    }
                    }


type alias Summary event =
    { events : List (SummaryEvent event)
    , now : Time.Absolute
    , startTime : Time.Absolute
    }


type SummaryEvent event
    = EventSummary event Time.Absolute ActualDuration
    | InterruptionSummary
        { target : event
        , targetTime : Time.Absolute
        , interruptedAt : Time.Absolute
        , newTarget : event
        , newTargetTime : Time.Absolute
        , newTargetDuration : ActualDuration
        }


type ActualDuration
    = OpenDuration
    | KnownDuration Time.Duration


{-| -}
type Frames item
    = Single item
    | Hold Int item
    | Walk item (List (Frames item))
    | WithRest (Resting item) (Frames item)


{-| -}
type Resting item
    = Cycle Period (List (Frames item))


captureTimeline :
    (state -> anchor)
    -> Timeline state
    -> Summary anchor
captureTimeline lookup (Timeline timelineDetails) =
    case timelineDetails.events of
        Timetable timetable ->
            case timetable of
                [] ->
                    -- I believe this case is fleeting because as soon as we have a real time,
                    -- we add a line to the timetable.
                    -- However, maybe it is awkwardly rendered once?
                    { events =
                        [ EventSummary
                            (lookup timelineDetails.initial)
                            timelineDetails.now
                            OpenDuration
                        ]
                    , now = timelineDetails.now
                    , startTime = timelineDetails.now
                    }

                (Line start startEv remain) :: remainingLines ->
                    let
                        events =
                            captureTimelineHelper lookup
                                (startEv :: remain)
                                remainingLines
                                []
                    in
                    { events = List.reverse events
                    , now = timelineDetails.now
                    , startTime = start
                    }


{-| Summarize all the events on the current timeline.

Note, this does not take into account time adjustments!

Essentially this is only used for sprite animation, which currently dont have leaveLate or arriveEarly.

-}
captureTimelineHelper :
    (state -> anchor)
    -> List (Occurring state)
    -> List (Line state)
    -> List (SummaryEvent anchor)
    -> List (SummaryEvent anchor)
captureTimelineHelper lookup events futureLines summary =
    -- futureStart starts a new line.
    -- if an interruption occurs, we want to interpolate to the point of the interruption
    -- then transition over to the new line.
    case events of
        [] ->
            case futureLines of
                [] ->
                    summary

                (Line futureStart futureStartEv futureRemain) :: restOfFuture ->
                    captureTimelineHelper lookup (futureStartEv :: futureRemain) restOfFuture summary

        (Occurring event start eventEnd) :: remain ->
            case futureLines of
                [] ->
                    case remain of
                        [] ->
                            let
                                newEvent =
                                    EventSummary (lookup event) start OpenDuration
                            in
                            newEvent :: summary

                        _ ->
                            let
                                newEvent =
                                    EventSummary (lookup event) start (KnownDuration (Time.duration start eventEnd))
                            in
                            captureTimelineHelper lookup remain futureLines (newEvent :: summary)

                (Line futureStart futureStartEv futureRemain) :: restOfFuture ->
                    if Time.thisBeforeOrEqualThat futureStart start then
                        -- interruption
                        let
                            newEvent =
                                InterruptionSummary
                                    { target = lookup event
                                    , targetTime = start
                                    , interruptedAt = futureStart
                                    , newTarget = lookup (getEvent futureStartEv)
                                    , newTargetTime = startTime futureStartEv
                                    , newTargetDuration =
                                        case futureRemain of
                                            [] ->
                                                case restOfFuture of
                                                    [] ->
                                                        OpenDuration

                                                    _ ->
                                                        KnownDuration (Time.duration start eventEnd)

                                            _ ->
                                                KnownDuration (Time.duration start eventEnd)
                                    }
                        in
                        captureTimelineHelper lookup futureRemain restOfFuture (newEvent :: summary)

                    else
                        -- queue up new events
                        let
                            newEvent =
                                EventSummary (lookup event)
                                    start
                                    (KnownDuration (Time.duration start eventEnd))
                        in
                        captureTimelineHelper lookup remain futureLines (newEvent :: summary)


{-| What is the last events /starting/ time
-}
findLastEventInLines first remaining =
    case remaining of
        [] ->
            case first of
                Line _ startEv rem ->
                    getLastEvent startEv rem

        r1 :: r1Remain ->
            findLastEventInLines r1 r1Remain


getLastEvent head rest =
    case rest of
        [] ->
            head

        top :: tail ->
            getLastEvent top tail


getFrames :
    Time.Absolute
    ->
        { perFrame : Time.Duration
        , offset : Float
        , startTime : Time.Absolute
        , endTime : Time.Absolute
        }
    -> (Time.Absolute -> motion)
    -> List (Frame motion)
    -> List (Frame motion)
getFrames currentTime config fn newFrames =
    if Time.thisBeforeOrEqualThat currentTime config.startTime then
        case newFrames of
            [] ->
                [ Frame 0 (fn config.startTime)
                , Frame 1 (fn config.endTime)
                ]

            _ ->
                Frame 0 (fn config.startTime)
                    :: newFrames

    else
        let
            new =
                Frame (Time.progress config.startTime config.endTime currentTime) (fn currentTime)
        in
        getFrames (Time.rollbackBy config.perFrame currentTime) config fn (new :: newFrames)


zeroDuration : Duration.Duration
zeroDuration =
    Duration.milliseconds 0



{- BOOKKEEPING -}


current : Timeline event -> event
current ((Timeline details) as timeline) =
    foldp
        identity
        { start =
            \_ ->
                details.initial
        , dwellFor =
            \cur duration ->
                cur
        , dwellPeriod = \_ -> Nothing
        , adjustor =
            \_ ->
                { arrivingEarly = 0
                , leavingLate = 0
                }
        , after =
            \lookup target future ->
                getEvent target
        , lerp = pass
        }
        timeline


pass _ _ target _ _ _ _ =
    target


getPrev _ maybePrevious target _ _ _ _ =
    case maybePrevious of
        Just p ->
            p

        Nothing ->
            target


type Status
    = Dwelling Time.Duration
    | Transitioning Float


previous : Timeline event -> event
previous ((Timeline details) as timeline) =
    foldp
        identity
        { start =
            \_ ->
                details.initial
        , dwellFor =
            \cur duration ->
                cur
        , dwellPeriod = \_ -> Nothing
        , adjustor =
            \_ ->
                { arrivingEarly = 0
                , leavingLate = 0
                }
        , after =
            \lookup target future ->
                getEvent target
        , lerp = getPrev
        }
        timeline


status : Timeline event -> Status
status ((Timeline details) as timeline) =
    foldp
        identity
        { start =
            \_ ->
                Transitioning 0
        , dwellFor =
            \cur duration ->
                Dwelling duration
        , dwellPeriod = \_ -> Nothing
        , adjustor =
            \_ ->
                { arrivingEarly = 0
                , leavingLate = 0
                }
        , after =
            \lookup target future ->
                Transitioning 1
        , lerp =
            \prevEndTime maybePrev target targetTime now maybeLookAhead state ->
                -- target
                -- Transitioning 0
                let
                    start =
                        prevEndTime

                    end =
                        targetTime
                in
                if now >= end then
                    Transitioning 1

                else
                    Transitioning (Time.progress (Time.millis start) (Time.millis end) (Time.millis now))
        }
        timeline


{--}
{-| The proportion (number between 0 and 1) of progress between the last state and the new one.

Once we arrive at a new state, this value will be 1 until we start another transition.

-}
progress : Timeline state -> Float
progress timeline =
    case status timeline of
        Dwelling _ ->
            1

        Transitioning t ->
            t


{-| The number of milliseconds that has occurred since we came to rest at the most recent state.

If we're in transition, this is 0.

-}
dwellingTime : Timeline state -> Float
dwellingTime timeline =
    case status timeline of
        Dwelling x ->
            Duration.inMilliseconds x

        Transitioning _ ->
            0



{- ANIMATOR -}
{- The animator checks to see if any timelines are running and also has the ability to update the animation state.


   Different animators can do different things


      - Normal -> always on
      - Inline -> on when moving (require anotation of dwelling events)
      - CSS    -> single update when timeline is updated


-}


{-| -}
type Animator model
    = Animator (model -> Bool) (Time.Posix -> model -> model)


{-| -}
needsUpdate : Timeline event -> Bool
needsUpdate ((Timeline timeline) as tl) =
    case timeline.queued of
        Nothing ->
            case timeline.interruption of
                [] ->
                    timeline.running

                _ ->
                    True

        Just _ ->
            True


{-| -}
hasChanged : Timeline event -> Bool
hasChanged (Timeline timeline) =
    case timeline.queued of
        Nothing ->
            case timeline.interruption of
                [] ->
                    False

                _ ->
                    True

        Just _ ->
            True


{-| -}
justInitialized : Timeline event -> Bool
justInitialized (Timeline timeline) =
    case timeline.now of
        Quantity.Quantity qty ->
            qty == 0



{- Oscillator preprocessing -}


pauseToBounds : Pause -> Time.Duration -> Time.Duration -> ( Float, Float )
pauseToBounds (Pause dur val) activeDuration totalDur =
    let
        start =
            Quantity.multiplyBy val activeDuration
    in
    ( Quantity.ratio start totalDur
    , Quantity.ratio (Quantity.plus start dur) totalDur
    )


pauseValue : Pause -> Float
pauseValue (Pause _ v) =
    v


prepareOscillator : Time.Duration -> List Pause -> (Float -> Float) -> ( Float -> Float, Time.Duration )
prepareOscillator activeDuration pauses osc =
    let
        -- total duration of the oscillation (active + pauses)
        totalDuration =
            List.foldl
                (\(Pause p _) d ->
                    Quantity.plus p d
                )
                activeDuration
                pauses

        {- u -> 0-1 of the whole oscillation, including pauses
           a -> 0-1 of the `active` oscillation, which does not include pausese
           ^ this is what we use for feeding the osc function.

           ps -> a list of pauses

        -}
        withPause u a ps =
            case ps of
                [] ->
                    osc a

                p :: [] ->
                    case pauseToBounds p activeDuration totalDuration of
                        ( start, end ) ->
                            if u >= start && u <= end then
                                -- this pause is currently happening
                                pauseValue p

                            else if u > end then
                                -- this pause already happend
                                -- "shrink" the active duration by the pause's duration
                                let
                                    pauseDuration =
                                        end - start
                                in
                                osc (a - pauseDuration)

                            else
                                -- this pause hasn't happened yet
                                osc a

                p :: lookahead :: remain ->
                    case pauseToBounds p activeDuration totalDuration of
                        ( start, end ) ->
                            if u >= start && u <= end then
                                -- this pause is currently happening
                                pauseValue p

                            else if u > end then
                                -- this pause already happend
                                -- "shrink" the active duration by the pause's duration
                                -- and possibly account for the gap between pauses.
                                let
                                    pauseDuration =
                                        end - start

                                    gap =
                                        -- this is the gap between pauses
                                        -- or "active" time
                                        --
                                        case pauseToBounds lookahead activeDuration totalDuration of
                                            ( nextPauseStart, nextPauseEnd ) ->
                                                if u >= nextPauseStart then
                                                    nextPauseStart - end

                                                else
                                                    0
                                in
                                withPause u ((a + gap) - pauseDuration) (lookahead :: remain)

                            else
                                -- this pause hasn't happened yet
                                osc a

        fn u =
            withPause u u pauses
    in
    ( fn, totalDuration )
