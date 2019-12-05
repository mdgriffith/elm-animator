module Internal.Timeline exposing
    ( Timeline(..), TimelineDetails, Occurring(..), getEvents
    , Interpolator
    , Schedule(..), Event(..)
    , foldp, update, needsUpdate
    , Phase(..), mapPhase
    )

{-|

@docs Timeline, TimelineDetails, Occurring, getEvents

@docs Interpolator

@docs Schedule, Event

@docs rewrite, after, between

@docs foldp, update, needsUpdate

-}

import Internal.Time as Time
import Quantity
import Time


{-| A list of events that haven't been added to the schedule yet.
-}
type Schedule event
    = Schedule Time.Duration (List (Event event))


{-| -}
type Event event
    = Event Time.Duration event (Maybe Time.Duration)


{-| -}
type Timeline event
    = Timeline (TimelineDetails event)


type alias TimelineDetails event =
    { initial : event
    , now : Time.Absolute
    , events : Timetable event
    , queued : Maybe (Schedule event)
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
    = Line Time.Absolute (List (Occurring event))


mapTable : (a -> b) -> Timetable a -> Timetable b
mapTable fn (Timetable lines) =
    Timetable (List.map (mapLine fn) lines)


mapLine : (a -> b) -> Line a -> Line b
mapLine fn (Line t els) =
    Line t (List.map fn els)


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
-- {-| -}
-- between : event -> event -> Timeline event -> Timeline Bool
-- between start end (Timeline tl) =
--     Timeline
--         { initial = start == tl.initial
--         , now = tl.now
--         , running = tl.running
--         , events =
--             List.foldl (betweenEvent start end) ( NotStarted, [] ) tl.events
--                 |> Tuple.second
--                 |> List.reverse
--         , queued = Nothing
--         }


type Between
    = NotStarted
    | Started
    | Done


betweenEvent : event -> event -> Occurring event -> ( Between, List (Occurring Bool) ) -> ( Between, List (Occurring Bool) )
betweenEvent startCheckpoint endCheckpoint (Occurring ev time maybeDwell) ( status, events ) =
    case status of
        NotStarted ->
            if startCheckpoint == ev then
                ( Started, Occurring True time maybeDwell :: events )

            else
                ( NotStarted, Occurring False time maybeDwell :: events )

        Started ->
            if startCheckpoint == ev then
                ( Started, Occurring True time maybeDwell :: events )

            else
                ( NotStarted, Occurring False time maybeDwell :: events )

        Done ->
            ( status, Occurring False time maybeDwell :: events )



-- {-| -}
-- after : event -> Timeline event -> Timeline Bool
-- after ev (Timeline tl) =
--     let
--         started =
--             tl.initial == ev
--     in
--     Timeline
--         { initial = started
--         , now = tl.now
--         , running = tl.running
--         , events =
--             List.foldl (afterEvent ev) ( started, [] ) tl.events
--                 |> Tuple.second
--                 |> List.reverse
--         , queued = Nothing
--         }


afterEvent : event -> Occurring event -> ( Bool, List (Occurring Bool) ) -> ( Bool, List (Occurring Bool) )
afterEvent checkpoint (Occurring ev time maybeDwell) ( isAfter, events ) =
    if isAfter then
        ( isAfter, Occurring isAfter time maybeDwell :: events )

    else if checkpoint == ev then
        ( True, Occurring True time maybeDwell :: events )

    else
        ( False, Occurring False time maybeDwell :: events )


{-| -}
needsUpdate : Timeline event -> Bool
needsUpdate (Timeline timeline) =
    (timeline.queued /= Nothing)
        || timeline.running


getEvents : Timeline event -> List ( Time.Posix, event )
getEvents (Timeline timeline) =
    List.map (\(Occurring evt time maybeDwell) -> ( Time.toPosix time, evt )) timeline.events


{-| -}
update : Time.Posix -> Timeline event -> Timeline event
update now (Timeline timeline) =
    Timeline
        { timeline
            | now = Time.absolute now
            , events =
                case timeline.queued of
                    Nothing ->
                        timeline.events

                    Just queuedSchedule ->
                        enqueue timeline (Time.absolute now) queuedSchedule
            , queued = Nothing
        }


{-| Interrupt a current timetable with a new list of events.

    - If this timeline is after all other timelines
        -> queue it to the end and extend the dwell of the last event
    - otherwise, add as a new `Line` to the timetable.

-}
interrupt : TimelineDetails events -> Time.Absolute -> Schedule events -> Timetable.Timetable events
interrupt details now (Schedule delay reverseQueued) =
    Debug.todo "Ugh"


{-| Queue a list of events to be played after everything.

    - add events to the timeline that is currently active.
    - if we're past all events,
        -> add additional dwell time to the last event.

-}
enqueue : TimelineDetails events -> Time.Absolute -> Schedule events -> Timetable.Timetable events
enqueue timeline now (Schedule delay reverseQueued) =
    let
        queued =
            List.reverse reverseQueued

        reversedEvents =
            List.reverse timeline.events

        start =
            Time.advanceBy delay now
    in
    case reversedEvents of
        [] ->
            List.foldl toOccurring ( start, [] ) queued
                |> Tuple.second
                |> List.reverse

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
                List.reverse (newLastEvent :: eventTail)
                    ++ newEvents

            else
                timeline.events ++ newEvents


addToDwell duration maybeDwell =
    case maybeDwell of
        Nothing ->
            Just duration

        Just existing ->
            Just (Quantity.plus duration existing)


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

    `Promoter` is a function that transforms an `anchor` into a `motion`.

    `Interpolator` is a function that maps linearly between two `motions`.

-}
foldp : (event -> anchor) -> Interpolator event anchor motion -> Timeline event -> motion
foldp lookup mobilize (Timeline timeline) =
    case timeline.events of
        [] ->
            -- Generally, this shouldn't be reachable because we require an event on initialization
            -- likely we'll want to change events to `(start, [remaining])` at some point in the future
            mobilize lookup (Occurring timeline.initial timeline.now Nothing) Nothing Start

        startingEvent :: _ ->
            .state <|
                List.foldl
                    (\_ cursor ->
                        if cursor.done then
                            cursor

                        else
                            case cursor.events of
                                [] ->
                                    cursor

                                target :: remaining ->
                                    case getPhase cursor.previous target timeline.now cursor.state of
                                        ( finished, phase ) ->
                                            { state =
                                                mobilize lookup target (List.head remaining) phase
                                            , events = remaining
                                            , previous = target
                                            , done = finished == Finished
                                            }
                    )
                    { state = mobilize lookup startingEvent (List.head (List.drop 1 timeline.events)) Start
                    , events = timeline.events
                    , previous = startingEvent
                    , done = List.isEmpty timeline.events
                    }
                    timeline.events


getPhase : Occurring event -> Occurring event -> Time.Absolute -> state -> ( DoneOr, Phase state )
getPhase (Occurring prev prevTime maybePrevDwell) (Occurring event eventTime maybeDwell) now state =
    let
        eventEndTime =
            case maybeDwell of
                Nothing ->
                    eventTime

                Just dwell ->
                    Time.advanceBy dwell eventTime
    in
    if Time.thisAfterThat now eventEndTime then
        ( NotDone, After state )

    else if Time.thisAfterThat now eventTime then
        let
            dwellDuration =
                Time.duration eventTime now
        in
        ( Finished, Resting dwellDuration state )

    else
        let
            prevEventEndTime =
                case maybePrevDwell of
                    Nothing ->
                        prevTime

                    Just dwell ->
                        Time.advanceBy dwell prevTime

            progressToNewEvent =
                Time.progress
                    prevEventEndTime
                    eventTime
                    now
        in
        ( Finished
        , TransitioningTo
            { percent = progressToNewEvent
            , previousTime = prevEventEndTime
            }
            state
        )


mapPhase : (a -> b) -> Phase a -> Phase b
mapPhase fn phase =
    case phase of
        Start ->
            Start

        After a ->
            After (fn a)

        TransitioningTo progress a ->
            TransitioningTo progress (fn a)

        Resting dur a ->
            Resting dur (fn a)


type Phase state
    = Start
      -- give me the state after the target event is finished
    | After state
      -- give me the state while transiting to the target event
    | TransitioningTo Progress state
      -- give me the state while the current event is ongoing
    | Resting Time.Duration state


type DoneOr
    = Finished
    | NotDone


type alias Interpolator event anchor state =
    (event -> anchor)
    -> Occurring event
    -> Maybe (Occurring event)
    -> Phase state
    -> state


type alias Progress =
    { percent : Float
    , previousTime : Time.Absolute
    }
