module Internal.Timeline exposing
    ( Timeline(..), TimelineDetails, Occurring(..), getEvents
    , Interpolator
    , Schedule(..), Event(..)
    , foldp, update, needsUpdate
    , startTime, endTime, getEvent, extendEventDwell, hasDwell
    , addToDwell
    , progress, dwellingTime
    , current, startPass, pass
    , Phase(..), Adjustment, Line(..), Timetable(..)
    , Capturing(..), Captured(..), mostRecentlyCaptured
    , foldpSlim
    , Description(..), Interp, Period(..), Previous(..), atTime, currentAndPrevious, gc, gcLog, getDwell, linesAreActive, previousEndTime, previousStartTime, updateNoGC
    )

{-|

@docs Timeline, TimelineDetails, Occurring, getEvents

@docs Interpolator

@docs Schedule, Event

@docs foldp, update, needsUpdate

@docs startTime, endTime, getEvent, extendEventDwell, hasDwell

@docs addToDwell

@docs progress, dwellingTime

@docs current, startPass, pass

@docs Phase, Adjustment, Line, Timetable

@docs Capturing, Captured, mostRecentlyCaptured

@docs foldpSlim

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


type alias DwellFor anchor state =
    anchor -> Time.Duration -> state


type alias After event anchor state =
    (event -> anchor) -> Occurring event -> List (Occurring event) -> state


type alias Lerp event anchor state =
    (event -> anchor) -> Previous event -> Occurring event -> List (Occurring event) -> Time.Absolute -> state -> state


type alias DwellPeriod anchor =
    anchor -> Maybe Period


type alias Start anchor state =
    anchor -> state


foldpSlim :
    Capturing
    -> (state -> anchor)
    -> Interp state anchor motion
    -> Timeline state
    -> motion
foldpSlim capturing lookup fn ((Timeline timelineDetails) as timeline) =
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
                    foldOverLinesSingle
                        fn
                        lookup
                        timelineDetails
                        firstLine
                        remainingLines
                        start


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


getDwell : Occurring event -> Maybe Time.Duration
getDwell (Occurring _ _ maybeDwell) =
    maybeDwell


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


previousStartTime : Previous event -> Time.Absolute
previousStartTime prev =
    case prev of
        Previous event ->
            startTime event

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
        | running = True --running: TODO: actually flag when active
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
    case events of
        [] ->
            ( endsAt, [ Occurring event occursAt maybeDwell ] )

        prev :: remain ->
            if startTime prev == occursAt then
                -- then this event would supercede the previous one
                ( endsAt, Occurring event occursAt maybeDwell :: remain )

            else
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
    Capturing
    -> (state -> anchor)
    -> Starter state anchor motion
    -> Maybe (TimeAdjustor anchor)
    -> Interpolator state anchor motion
    -> Timeline state
    -> Captured motion
foldp capturing lookup starter maybeAdjustTiming interpolate ((Timeline timelineDetails) as timeline) =
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
            foldOverLines capturing Beginning lookup maybeAdjustTiming interpolate timelineDetails startingCursor timetable Nothing


type Capturing
    = CaptureNow
      --              frames per second to capture
    | CaptureFuture Float


{-| -}
type Beginning
    = Beginning
    | Continuing


type Captured motion
    = Single motion
    | Future
        { fps : Float
        , mostRecent : motion
        , frames : List motion
        , dwell :
            Maybe
                { iterations : Maybe Int
                , frames : List motion
                }
        }


mostRecentlyCaptured : Captured motion -> motion
mostRecentlyCaptured cap =
    case cap of
        Single m ->
            m

        Future details ->
            details.mostRecent


getFuture :
    Captured motion
    ->
        { frames : List motion
        , dwell :
            Maybe
                { iterations : Maybe Int
                , frames : List motion
                }
        }
getFuture cap =
    case cap of
        Single thing ->
            { frames = [ thing ]
            , dwell = Nothing
            }

        Future future ->
            { frames = future.frames
            , dwell = future.dwell
            }


type alias Cursor event state =
    { state : state
    , captured : Maybe (Captured state)
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


type alias Interp state anchor motion =
    { start : anchor -> motion
    , adjustor : TimeAdjustor anchor
    , dwellFor : DwellFor anchor motion
    , dwellPeriod : DwellPeriod anchor
    , after : After state anchor motion
    , lerp : Lerp state anchor motion
    }


foldOverLinesSingle :
    Interp state anchor motion
    -> (state -> anchor)
    -> TimelineDetails state
    -- current line
    -> Line state
    -> List (Line state)
    -> motion
    -> motion
foldOverLinesSingle fn lookup details (Line lineStart lineStartEv lineRemain) futureLines state =
    -- futureStart starts a new line.
    -- if an interruption occurs, we want to interpolate to the point of the interruption
    -- then transition over to the new line.
    let
        transition newState =
            case futureLines of
                [] ->
                    newState

                ((Line futureStart futureStartEv futureRemain) as future) :: restOfFuture ->
                    if Time.thisBeforeThat futureStart details.now then
                        foldOverLinesSingle fn lookup details future restOfFuture newState

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
    in
    let
        eventStartTime =
            startTime lineStartEv
    in
    if Time.thisBeforeThat now eventStartTime then
        -- lerp from state to lineStartEv
        -- *Note*, the occurring event we're creating here probably wants soemthing like details.originTime and a real dwell time.
        -- If we're coming in from a transition
        fn.lerp lookup
            (Previous (Occurring details.initial lineStart Nothing))
            lineStartEv
            lineRemain
            now
            state
            |> transition

    else
        let
            eventEndTime =
                endTime lineStartEv
        in
        if Time.thisAfterOrEqualThat now eventEndTime then
            -- after linestartEv
            case lineRemain of
                [] ->
                    -- dwell at lineStartEv, there's nothing to transition to
                    fn.dwellFor (lookup (getEvent lineStartEv)) (Time.duration eventStartTime now)
                        |> transition

                next :: lineRemain2 ->
                    if Time.thisBeforeThat now (startTime next) then
                        -- Before next.starTime
                        --     -> lerp start to next
                        fn.lerp
                            lookup
                            (Previous lineStartEv)
                            next
                            lineRemain2
                            now
                            state
                            |> transition

                    else if Time.thisBeforeThat now (endTime next) then
                        -- After next.startTime
                        --- Before next.endTime
                        --      -> we're dwelling at `next`
                        fn.dwellFor
                            (lookup (getEvent next))
                            (Time.duration (startTime next) now)
                            |> transition

                    else
                        -- After lineStart.endTime
                        -- After next.startTime
                        -- After next.endTime
                        case lineRemain2 of
                            [] ->
                                -- Nothing to continue on to,
                                --      -> we're dwelling at `next`
                                fn.dwellFor (lookup (getEvent next))
                                    (Time.duration (startTime next) now)
                                    |> transition

                            next2 :: lineRemain3 ->
                                -- continue on
                                if Time.thisBeforeThat now (startTime next2) then
                                    let
                                        after =
                                            fn.after lookup next lineRemain2
                                    in
                                    fn.lerp
                                        lookup
                                        (Previous next)
                                        next2
                                        lineRemain3
                                        now
                                        after
                                        |> transition

                                else if Time.thisBeforeThat now (endTime next2) then
                                    -- we're dwelling at `next2`
                                    fn.dwellFor
                                        (lookup (getEvent next2))
                                        (Time.duration (startTime next2) now)
                                        |> transition

                                else
                                    let
                                        after =
                                            fn.after lookup next2 lineRemain3
                                    in
                                    foldOverLinesSingle
                                        fn
                                        lookup
                                        details
                                        (Line (endTime next) next2 lineRemain3)
                                        futureLines
                                        after

        else
            -- dwell at linestartEv
            -- we've checked that it's not after or before, so it has to be between the start and end
            case lineStartEv of
                Occurring _ _ maybeDwell ->
                    case maybeDwell of
                        Nothing ->
                            fn.dwellFor (lookup (getEvent lineStartEv)) zeroDuration
                                |> transition

                        Just dur ->
                            fn.dwellFor (lookup (getEvent lineStartEv)) dur
                                |> transition


capture : Captured motion -> motion -> Captured motion
capture cap new =
    case cap of
        Single _ ->
            Single new

        Future details ->
            Future
                { fps = details.fps
                , mostRecent = new
                , frames = details.frames
                , dwell = details.dwell
                }


foldOverLines :
    Capturing
    -> Beginning
    -> (event -> anchor)
    -> Maybe (TimeAdjustor anchor)
    -> Interpolator event anchor motion
    -> TimelineDetails event
    -> motion
    -> List (Line event)
    -> Maybe (Cursor event motion)
    -> Captured motion
foldOverLines capturing beginning lookup maybeAdjustor interpolate timeline startingState lines existingCursor =
    case lines of
        [] ->
            -- Generally, this shouldn't be reachable because we require an event on initialization
            -- likely we'll want to change events to `(start, [remaining])` at some point in the future
            -- interpolate lookup (Occurring timeline.initial timeline.now Nothing) Nothing Start
            case existingCursor of
                Nothing ->
                    Single startingState

                Just exist ->
                    exist.captured
                        |> Maybe.withDefault
                            (Single exist.state)

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
                        (overEvents capturing timeline.now maybeInterruption lookup maybeAdjustor interpolate)
                        (case existingCursor of
                            Nothing ->
                                { state = startingState
                                , captured = Nothing
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
                case capturing of
                    CaptureNow ->
                        Single cursor.state

                    CaptureFuture fps ->
                        -- let
                        --     _ =
                        --         logIndent 4 "-------->" " forward for capture"
                        -- in
                        foldOverLines capturing
                            cursor.beginning
                            lookup
                            maybeAdjustor
                            interpolate
                            timeline
                            cursor.state
                            remaining
                            (Just cursor)
                -- cursor.captured
                --     |> Maybe.withDefault (Single cursor.state)

            else
                foldOverLines capturing
                    cursor.beginning
                    lookup
                    maybeAdjustor
                    interpolate
                    timeline
                    cursor.state
                    remaining
                    (Just cursor)


{-| #1 - foldOverLines - if phase == finished && capturing ~= CaptureFuture then

    - continue to fold over all lines

#2 - overEvents - if phase == fininished && capturing ~= CaptureFuture then

    - if Finished ->
        if CaptureFuture, then
            capture all events until interrupted
        else
            cursor
    if NotDone then

        if we Become finished && capturing ~= CaptureFuture:
            -> Roll forward and collect all frames

-}
overEvents :
    Capturing
    -> Time.Absolute
    -> Maybe Time.Absolute
    -> (event -> anchor)
    -> Maybe (TimeAdjustor anchor)
    -> Interpolator event anchor motion
    -> Occurring event
    -> Cursor event motion
    -> Cursor event motion
overEvents capturing now maybeInterruption lookup maybeAdjustor interpolate _ cursor =
    -- let
    -- interrupted =
    --     case maybeInterruption of
    --         Nothing ->
    --             False
    --         Just interruptTime ->
    --             Time.thisAfterOrEqualThat (previousEndTime cursor.previous) interruptTime
    -- _ =
    --     logSpace "Over events" ( cursor.status, cursor.previous, List.head cursor.events )
    -- _ =
    --     logIndent 4 "now, interruption" ( now, maybeInterruption )
    -- in
    case cursor.status of
        Finished ->
            case capturing of
                CaptureNow ->
                    cursor

                CaptureFuture fps ->
                    let
                        interrupted =
                            -- Debug.log "finished, interrupted" <|
                            case maybeInterruption of
                                Nothing ->
                                    False

                                Just interruptTime ->
                                    Time.thisAfterThat (previousEndTime cursor.previous) interruptTime
                    in
                    if interrupted then
                        cursor

                    else
                        let
                            eventEndTime =
                                maybeInterruption
                                    |> Maybe.withDefault
                                        (List.head cursor.events
                                            |> Maybe.map endTime
                                            -- is now correct here?
                                            |> Maybe.withDefault now
                                        )

                            newCursor =
                                overEventsHelper
                                    capturing
                                    maybeInterruption
                                    lookup
                                    maybeAdjustor
                                    interpolate
                                    now
                                    cursor

                            -- _ =
                            --     Debug.log "times" ( previousEndTime cursor.previous, eventEndTime )
                            -- _ =
                            --     Debug.log "capture events with" cursor
                            -- _ =
                            --     Debug.log "newcursor" newCursor
                            framesTillEndOfEvent =
                                framesBetween fps (previousEndTime cursor.previous) eventEndTime

                            -- |> Debug.log "---Finished-------------------> frame count"
                            iterator =
                                overEventsHelper
                                    capturing
                                    maybeInterruption
                                    lookup
                                    maybeAdjustor
                                    interpolate

                            frames =
                                List.foldl
                                    (captureEvents fps iterator (previousEndTime cursor.previous) cursor)
                                    []
                                    (List.range 1 (floor framesTillEndOfEvent))
                                    |> List.reverse

                            -- |> logIndent 4 "----> found frames"
                        in
                        { newCursor
                            | captured =
                                Just
                                    (mergeCaptures
                                        { mostRecent = newCursor.state
                                        , fps = fps
                                        , frames = newCursor.state :: frames
                                        , dwell = Nothing
                                        }
                                        cursor.captured
                                    )
                        }

        Interrupted ->
            cursor

        NotDone ->
            let
                newCursor =
                    overEventsHelper
                        capturing
                        maybeInterruption
                        lookup
                        maybeAdjustor
                        interpolate
                        now
                        cursor
            in
            if newCursor.status == Finished then
                case capturing of
                    CaptureNow ->
                        newCursor

                    CaptureFuture fps ->
                        -- capture events till the end or interrupted
                        let
                            interrupted =
                                -- Debug.log "ND, interrupted" <|
                                case maybeInterruption of
                                    Nothing ->
                                        False

                                    Just interruptTime ->
                                        Time.thisAfterOrEqualThat now interruptTime
                        in
                        -- if logIndent 8 "SKIPPING" interrupted then
                        if interrupted then
                            newCursor

                        else
                            let
                                eventEndTime =
                                    maybeInterruption
                                        |> Maybe.withDefault
                                            (List.head cursor.events
                                                |> Maybe.map endTime
                                                -- is now correct here?
                                                |> Maybe.withDefault now
                                            )

                                framesTillEndOfEvent =
                                    framesBetween fps now eventEndTime

                                -- |> logIndent 4 "ND -> frame count"
                                -- _ =
                                -- logIndent 4 "ND times" ( now, eventEndTime )
                                iterator =
                                    overEventsHelper
                                        capturing
                                        maybeInterruption
                                        lookup
                                        maybeAdjustor
                                        interpolate

                                frames =
                                    List.foldl
                                        (captureEvents fps iterator now cursor)
                                        []
                                        (List.range 1 (floor framesTillEndOfEvent))
                                        |> List.reverse

                                -- |> logIndent 4 "ND ----> found frames"
                                cursorAtEndOfEvent =
                                    overEventsHelper
                                        capturing
                                        maybeInterruption
                                        lookup
                                        maybeAdjustor
                                        interpolate
                                        eventEndTime
                                        cursor

                                -- _ =
                                --     logIndent 4
                                --         "prevs"
                                --         ( cursor.previous, newCursor.previous, cursorAtEndOfEvent.previous )
                                -- |> Debug.log "cursor at end"
                            in
                            { --newCursor
                              cursorAtEndOfEvent
                                | captured =
                                    Just
                                        (Future
                                            { mostRecent = newCursor.state
                                            , fps = fps
                                            , frames = frames
                                            , dwell = Nothing
                                            }
                                        )
                            }

            else
                newCursor


logSpace label val =
    -- let
    --     _ = Debug.log "\n" " "
    -- in
    let
        x =
            Debug.log ("{-\n    " ++ label) val

        _ =
            Debug.log "\n-}" ""
    in
    val


logIndent n label val =
    Debug.log (String.repeat n " " ++ label) val


mergeCaptures details maybeCaptured =
    case maybeCaptured of
        Nothing ->
            Future details

        Just (Single motion) ->
            Future details

        Just (Future motion) ->
            -- let
            --     _ =
            --         Debug.log "merge frames" ( motion.frames, details.frames )
            -- in
            Future { details | frames = motion.frames ++ details.frames }


captureEvents fps fn now cursor frameIndex frames =
    let
        -- _ =
        --     Debug.log "             : cap -> " ( now, newNow )
        newNow =
            now
                |> Time.advanceBy (Duration.seconds ((1 / fps) * toFloat frameIndex))

        newCursor =
            fn newNow cursor
    in
    newCursor.state :: frames


framesBetween fps start end =
    let
        seconds =
            Time.duration start end
                |> Duration.inSeconds
    in
    seconds * fps


overEventsHelper :
    Capturing
    -> Maybe Time.Absolute
    -> (event -> anchor)
    -> Maybe (TimeAdjustor anchor)
    -> Interpolator event anchor motion
    -> Time.Absolute
    -> Cursor event motion
    -> Cursor event motion
overEventsHelper capturing maybeInterruption lookup maybeAdjustor interpolate now cursor =
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

                ( status, phase, currentTime ) =
                    getPhase cursor.beginning adjustedTarget now maybeInterruption

                newState =
                    interpolate
                        lookup
                        cursor.previous
                        adjustedTarget
                        adjustedLookAhead
                        phase
                        currentTime
                        cursor.state
            in
            { state =
                newState
            , captured =
                case capturing of
                    CaptureNow ->
                        Nothing

                    CaptureFuture fps ->
                        if status == Finished then
                            -- gather all frames
                            Nothing

                        else
                            Nothing
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
applyAdjustment :
    (event -> anchor)
    -> TimeAdjustor anchor
    ->
        { previousEventTime : Time.Absolute
        , applied : Adjustment
        }
    -> Occurring event
    -> Maybe (Occurring event)
    -> ( Occurring event, Maybe Adjustment )
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


current : Timeline event -> Occurring event
current timeline =
    mostRecentlyCaptured <|
        foldp
            CaptureNow
            identity
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


currentAndPrevious :
    Timeline event
    ->
        { previous : Previous event
        , previousTarget : Occurring event
        , target : Occurring event
        }
currentAndPrevious timeline =
    mostRecentlyCaptured <|
        foldp
            CaptureNow
            identity
            startCurrentAndPrevious
            Nothing
            passCurrentAndPrevious
            timeline


startCurrentAndPrevious :
    (event -> event)
    -> Occurring event
    ->
        { previous : Previous event
        , previousTarget : Occurring event
        , target : Occurring event
        }
startCurrentAndPrevious lookup start =
    { previous = Previous start
    , previousTarget = start
    , target = start
    }


passCurrentAndPrevious :
    (event -> event)
    -> Previous event
    -> Occurring event
    -> Maybe (Occurring event)
    -> Phase
    -> Time.Absolute
    ->
        { previous : Previous event
        , previousTarget : Occurring event
        , target : Occurring event
        }
    ->
        { previous : Previous event
        , previousTarget : Occurring event
        , target : Occurring event
        }
passCurrentAndPrevious _ prev target _ _ _ existing =
    { previous = prev
    , previousTarget = existing.target
    , target = target
    }


{--}
{-| The proportion (number between 0 and 1) of progress between the last state and the new one.

Once we arrive at a new state, this value will be 1 until we start another transition.

-}
progress : Timeline state -> Float
progress ((Timeline deets) as tl) =
    let
        cursor =
            currentAndPrevious tl

        start =
            case cursor.previous of
                Previous prev ->
                    endTime prev

                PreviouslyInterrupted interruptionTime ->
                    interruptionTime

        end =
            startTime cursor.target
    in
    if Time.thisAfterOrEqualThat deets.now end then
        1

    else
        Time.progress start end deets.now


{-| The number of milliseconds that has occurred since we came to rest at the most recent state.

If we're in transition, this is 0.

-}
dwellingTime : Timeline state -> Float
dwellingTime ((Timeline deets) as tl) =
    let
        cursor =
            currentAndPrevious tl
    in
    if Time.thisAfterOrEqualThat deets.now (endTime cursor.target) then
        Duration.inMilliseconds
            (Time.duration (startTime cursor.target) deets.now)

    else if Time.thisAfterOrEqualThat deets.now (startTime cursor.target) then
        case getDwell cursor.target of
            Nothing ->
                0

            Just dwell ->
                Duration.inMilliseconds dwell

    else
        0
