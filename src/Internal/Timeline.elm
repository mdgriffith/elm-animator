module Internal.Timeline exposing
    ( Timeline(..), TimelineDetails, Occurring(..), getEvents
    , Interpolator
    , Schedule(..), Event(..)
    , after, between
    , foldp, update, needsUpdate
    , Line(..), Phase(..), Timetable(..), addToDwell, endTime, extendEventDwell, mapPhase
    )

{-|

@docs Timeline, TimelineDetails, Occurring, getEvents

@docs Interpolator

@docs Schedule, Event

@docs rewrite, after, between

@docs foldp, update, needsUpdate

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


extendEventDwell : Time.Duration -> Event event -> Event event
extendEventDwell extendBy ((Event at ev maybeDwell) as thisEvent) =
    if Duration.inMilliseconds extendBy == 0 then
        thisEvent

    else
        Event at ev (addToDwell extendBy maybeDwell)


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


{-| When the event occurs, and how long we're dwelling at this event (or no dwelling at all)
-}
type Occurring event
    = Occurring event Time.Absolute (Maybe Time.Duration)


filterMapOccurring : (event -> Maybe newEvent) -> Occurring event -> Maybe (Occurring newEvent)
filterMapOccurring fn (Occurring ev time maybeDwell) =
    case fn ev of
        Nothing ->
            Nothing

        Just newEv ->
            Just (Occurring newEv time maybeDwell)



-- {-| -}
-- rewrite : newEvent -> Timeline event -> (event -> Maybe newEvent) -> Timeline newEvent
-- rewrite newStart (Timeline tl) newLookup =
--     Timeline
--         { initial = newStart
--         , now = tl.now
--         , running = tl.running
--         , events =
--             Timetable. (List.filterMap (filterMapOccurring newLookup)) tl.events
--         , queued = Nothing
--         }


{-| -}
between : event -> event -> Timeline event -> Timeline Bool
between start end (Timeline tl) =
    let
        isStarted =
            tl.initial == start

        initialState =
            if isStarted then
                -- TODO: pretty sure this should be the starting time of the whole timeline
                StartedAt (Time.absolute (Time.millisToPosix 0)) Nothing

            else
                NotStarted
    in
    Timeline
        { initial = isStarted
        , now = tl.now
        , running = tl.running
        , events =
            Tuple.first
                (mapTableWith
                    (betweenEvent start end)
                    initialState
                    tl.events
                )
        , queued = Nothing -- TODO: carry over queues and interruptions
        , interruption = []
        }


type Between
    = NotStarted
      -- starting time, and ending time.
    | StartedAt Time.Absolute (Maybe Time.Absolute)


betweenEvent : event -> event -> Occurring event -> Between -> ( Occurring Bool, Between )
betweenEvent startCheckpoint endCheckpoint (Occurring ev time maybeDwell) status =
    case status of
        NotStarted ->
            if startCheckpoint == ev then
                ( Occurring True time maybeDwell
                , StartedAt time Nothing
                )

            else
                ( Occurring False time maybeDwell
                , status
                )

        StartedAt startedAt maybeEnd ->
            if Time.thisAfterThat time startedAt then
                case maybeEnd of
                    Nothing ->
                        ( Occurring True time maybeDwell
                        , if ev == endCheckpoint then
                            StartedAt startedAt (Just time)

                          else
                            status
                        )

                    Just end ->
                        if Time.thisAfterThat time end then
                            ( Occurring False time maybeDwell
                            , status
                            )

                        else
                            ( Occurring True time maybeDwell
                            , status
                            )

            else
                ( Occurring False time maybeDwell
                , status
                )


{-| -}
after : event -> Timeline event -> Timeline Bool
after ev (Timeline tl) =
    let
        isStarted =
            tl.initial == ev

        initialState =
            if isStarted then
                -- TODO: pretty sure this should be the starting time of the whole timeline
                Just (Time.absolute (Time.millisToPosix 0))

            else
                Nothing
    in
    Timeline
        { initial = isStarted
        , now = tl.now
        , events =
            Tuple.first
                (mapTableWith
                    (afterEvent ev)
                    initialState
                    tl.events
                )

        -- TODO: these should be added
        , interruption = [] --tl.interruption
        , queued = Nothing -- tl.queued
        , running = tl.running
        }


afterEvent : event -> Occurring event -> Maybe Time.Absolute -> ( Occurring Bool, Maybe Time.Absolute )
afterEvent checkpoint (Occurring ev time maybeDwell) maybeCheckpointTime =
    case maybeCheckpointTime of
        Nothing ->
            if checkpoint == ev then
                ( Occurring True time maybeDwell
                , Just time
                )

            else
                ( Occurring False time maybeDwell
                , maybeCheckpointTime
                )

        Just checkpointAt ->
            ( Occurring (Time.thisAfterThat time checkpointAt) time maybeDwell
            , maybeCheckpointTime
            )


endTime : Occurring event -> Time.Absolute
endTime (Occurring _ time maybeDwell) =
    case maybeDwell of
        Nothing ->
            time

        Just dwell ->
            Time.advanceBy dwell time


{-| -}
needsUpdate : Timeline event -> Bool
needsUpdate (Timeline timeline) =
    (timeline.queued /= Nothing)
        || timeline.running


getEvents : Timeline event -> List ( Time.Posix, event )
getEvents (Timeline timeline) =
    case timeline.events of
        Timetable lines ->
            lines
                |> List.concatMap (\(Line _ start ev) -> start :: ev)
                |> List.map (\(Occurring evt time maybeDwell) -> ( Time.toPosix time, evt ))


getEnd : Timeline event -> Time.Posix
getEnd timeline =
    Time.millisToPosix 0


getStart : Timeline event -> Time.Posix
getStart timeline =
    Time.millisToPosix 0


{-| -}
update : Time.Posix -> Timeline event -> Timeline event
update now (Timeline timeline) =
    { timeline | now = Time.absolute now }
        |> applyQueued
        |> applyInterruptions
        |> Timeline


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

        i :: remaining ->
            let
                newEvents =
                    interrupt timeline timeline.now i
            in
            applyInterruptionHelper remaining { timeline | events = newEvents }


{-| Interrupt a current timetable with a new list of events.

    - If this timeline is after all other timelines
        -> queue it to the end and extend the dwell of the last event
    - otherwise, add as a new `Line` to the timetable.

-}
interrupt : TimelineDetails events -> Time.Absolute -> Schedule events -> Timetable events
interrupt details now ((Schedule delay startingEvent reverseQueued) as scheduled) =
    case details.events of
        Timetable lines ->
            case getLastEventTime lines of
                Nothing ->
                    enqueue details now scheduled

                Just last ->
                    if Time.thisAfterThat now last then
                        enqueue details now scheduled

                    else
                        Timetable (lines ++ [ createLine now scheduled ])


getLastEventTime : List (Line event) -> Maybe Time.Absolute
getLastEventTime lines =
    case List.head (List.reverse lines) of
        Nothing ->
            Nothing

        Just (Line startTime startEvent trailing) ->
            case List.reverse trailing of
                [] ->
                    Just startTime

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
                    if Time.thisAfterThat now startOne && Time.thisAfterThat startTwo now then
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
createLine now (Schedule delay (Event dur startEvent dwell) reverseQueued) =
    let
        start =
            Time.advanceBy delay now

        events =
            List.foldl toOccurring ( start, [] ) (List.reverse reverseQueued)
                |> Tuple.second
                |> List.reverse
    in
    Line now (Occurring startEvent now dwell) events


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
                startingEventWithDwell =
                    extendDwell delay startingEvent
            in
            List.foldl toOccurring ( start, [] ) queued
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
            if Time.thisAfterThat start lastEventTime then
                -- if the start of the scheduled events is after the last event time,
                -- then we need to increase the dwell time of the last event to match the start time.
                let
                    newLastEvent =
                        Occurring lastEvent
                            lastEventTime
                            (Just (Time.duration start lastEventTime))
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
    case maybeDwell of
        Nothing ->
            Just duration

        Just existing ->
            Just (Quantity.plus duration existing)


getOccurringTime (Occurring _ t _) =
    t


{-| Ok, we have a few subtle concepts here.

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
foldp : (event -> anchor) -> Starter event anchor motion -> Interpolator event anchor motion -> Timeline event -> motion
foldp lookup starter interpolate ((Timeline timelineDetails) as timeline) =
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
                    -- interpolate lookup startingEvent maybeNextEvent Start
                    starter lookup startingEvent
            in
            foldOverLines Beginning lookup interpolate timelineDetails startingCursor timetable


{-| There's some subtlty to starting the fold over timelines.

We always want a `previous event` to be available because it's important to a lot of calculations that involve adjusting the timeline last minute.

SO, how do we start everything?

-}
type Beginning
    = Beginning
    | Continuing


foldOverLines : Beginning -> (event -> anchor) -> Interpolator event anchor motion -> TimelineDetails event -> motion -> List (Line event) -> motion
foldOverLines beginning lookup interpolate timeline startingCursor lines =
    case lines of
        [] ->
            -- Generally, this shouldn't be reachable because we require an event on initialization
            -- likely we'll want to change events to `(start, [remaining])` at some point in the future
            -- interpolate lookup (Occurring timeline.initial timeline.now Nothing) Nothing Start
            startingCursor

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
                        (overEvents beginning timeline.now maybeInterruption lookup interpolate)
                        { state = startingCursor
                        , events = events
                        , previous = startingEvent
                        , status =
                            if List.isEmpty events then
                                Finished

                            else
                                NotDone
                        }
                        (startingEvent :: events)
            in
            if cursor.status == Finished then
                cursor.state

            else
                foldOverLines Continuing lookup interpolate timeline cursor.state remaining


type alias Cursor event state =
    { state : state
    , events : List (Occurring event)
    , previous : Occurring event
    , status : Status
    }


type Status
    = Finished
    | NotDone
    | Interrupted


overEvents :
    Beginning
    -> Time.Absolute
    -> Maybe Time.Absolute
    -> (event -> anchor)
    -> Interpolator event anchor motion
    -> Occurring event
    -> Cursor event motion
    -> Cursor event motion
overEvents beginning now maybeInterruption lookup interpolate _ cursor =
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
                    case getPhase beginning cursor.previous target now maybeInterruption cursor.state of
                        ( status, phase ) ->
                            { state =
                                interpolate lookup target (List.head remaining) phase
                            , events = remaining
                            , previous = target
                            , status = status
                            }


{-|

    Params:
        1. Previous Event
        2. Current Event
        3. Now
        4. Maybe interruption time
        5. state

When we get an interruption, we want to skip all events.

-}
getPhase : Beginning -> Occurring event -> Occurring event -> Time.Absolute -> Maybe Time.Absolute -> state -> ( Status, Phase event state )
getPhase beginning ((Occurring prev prevTime maybePrevDwell) as previousEvent) (Occurring event eventTime maybeDwell) now maybeInterruption state =
    let
        eventEndTime =
            case maybeDwell of
                Nothing ->
                    eventTime

                Just dwell ->
                    Time.advanceBy dwell eventTime

        interrupted =
            case maybeInterruption of
                Nothing ->
                    False

                Just interruptTime ->
                    Time.thisBeforeThat interruptTime now
    in
    case beginning of
        Beginning ->
            ( NotDone
              -- or Finished
            , Start
            )

        Continuing ->
            if interrupted then
                let
                    interruptionTime =
                        Maybe.withDefault now maybeInterruption
                in
                ( Interrupted
                , TransitioningTo
                    previousEvent
                    interruptionTime
                    state
                )

            else if Time.thisAfterThat now eventEndTime then
                ( NotDone, After previousEvent state )

            else if Time.thisAfterThat now eventTime then
                let
                    dwellDuration =
                        Time.duration eventTime now
                in
                ( Finished, Resting previousEvent dwellDuration state )

            else
                ( Finished
                , TransitioningTo
                    previousEvent
                    now
                    state
                )


mapPhase : (a -> b) -> Phase ev a -> Phase ev b
mapPhase fn phase =
    case phase of
        Start ->
            Start

        After prev a ->
            After prev (fn a)

        TransitioningTo prev t a ->
            TransitioningTo prev t (fn a)

        Resting prev dur a ->
            Resting prev dur (fn a)


type Phase event state
    = Start
      -- give me the state after the target event is finished
    | After (Previous event) state
      -- previous event, current time, previous state.
    | TransitioningTo (Previous event) Time.Absolute state
      -- give me the state while the current event is ongoing
    | Resting (Previous event) Time.Duration state


type alias Starter event anchor state =
    (event -> anchor)
    -> Occurring event
    -> state


type alias Interpolator event anchor state =
    (event -> anchor)
    -> Occurring event
    -> Maybe (Occurring event)
    -> Phase event state
    -> state


type alias Previous event =
    Occurring event
