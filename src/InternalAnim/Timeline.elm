module InternalAnim.Timeline exposing
    ( Timeline(..), TimelineDetails, Occurring(..)
    , Schedule(..), Event(..)
    , update, updateWith
    , startTime, endTime, getEvent, extendEventDwell
    , addToDwell
    , current, arrivedAt, arrived, previous, upcoming
    , progress
    , Line(..), Timetable(..)
    , foldpAll
    , gc, atTime, dwellingTime, getCurrentTime
    , Transition
    , getUpdatedAt, maxDelay, transitionProgress
    )

{-|

@docs Timeline, TimelineDetails, Occurring

@docs Schedule, Event

@docs update, updateWith

@docs startTime, endTime, getEvent, extendEventDwell

@docs addToDwell

@docs current, arrivedAt, arrived, previous, upcoming

@docs progress

@docs Line, Timetable

@docs foldpAll

@docs gc, atTime, dwellingTime, getCurrentTime

@docs Transition

-}

import InternalAnim.Duration as Duration
import InternalAnim.Quantity as Quantity
import InternalAnim.Time as Time
import Time


{-| A list of events that haven't been added to the schedule yet.
-}
type Schedule event
    = Schedule Time.Duration (Event event) (List (Event event))


{-| -}
type Event event
    = Event Time.Duration event (Maybe Time.Duration)


currentScheduleTarget : Schedule event -> event
currentScheduleTarget (Schedule _ (Event _ target _) _) =
    target


scheduleDelay : Schedule state -> Time.Duration
scheduleDelay (Schedule d _ _) =
    d


{-| -}
type Timeline event
    = Timeline (TimelineDetails event)


type alias TimelineDetails event =
    { -- The state preceding retained history, advanced when history is collected.
      initial : event

    -- The current wall time
    , now : Time.Absolute

    -- The last time the schedule or its retained history changed. CSS renders
    -- relative to this time; ordinary clock ticks leave it unchanged.
    , updatedAt : Time.Absolute
    , delay : Time.Duration
    , scale : Float
    , events : Timetable event
    , queued : Maybe (Schedule event)
    , interruption : List (Schedule event)

    -- Running means that there are ongoing events
    , running : Bool
    }


{-| A time table is a list of timelines that will occur.

Events proceed from earlier to later.

Lines are ordered earliest to latest.

-}
type Timetable event
    = Timetable (List (Line event))


{-| -- maybe previous event, starting time, starting event, subsequent events
-- The maybe previous event is only used to calculate time adjustments for arriveEarly and leaveLate
-}
type Line event
    = Line Time.Absolute (Occurring event) (List (Occurring event))


{-| When the event occurs and the end
-}
type Occurring event
    = Occurring event Time.Absolute Time.Absolute



{- TYPES FOR INTERPOLATION -}


type alias Transition state anchor motion =
    (state -> anchor)
    -- target event
    -> Occurring state
    -- now
    -> Time.Absolute
    -- start time:
    -- Either the end time of the previous transition
    -- or when the interruption happened
    -> Time.Absolute
    -- end time:
    -- Either the arrival time of the target or an earlier interruption.
    -> Time.Absolute
    -- Remaining events on this line; a later interruption may cancel them.
    -> List (Occurring state)
    -> motion
    -> motion


getEvent : Occurring event -> event
getEvent (Occurring ev _ _) =
    ev


extendEventDwell : Time.Duration -> Event event -> Event event
extendEventDwell extendBy ((Event at ev maybeDwell) as thisEvent) =
    if Duration.inMilliseconds extendBy == 0 then
        thisEvent

    else
        Event at ev (addToDwell extendBy maybeDwell)


startTime : Occurring event -> Time.Absolute
startTime (Occurring _ time _) =
    time


endTime : Occurring event -> Time.Absolute
endTime (Occurring _ _ end) =
    end


atTime : Time.Posix -> Timeline event -> Timeline event
atTime now (Timeline timeline) =
    Timeline { timeline | now = Time.absolute now }


getUpdatedAt : Timeline event -> Time.Absolute
getUpdatedAt (Timeline details) =
    details.updatedAt


getCurrentTime : Timeline event -> Time.Absolute
getCurrentTime (Timeline timeline) =
    Time.rollbackBy timeline.delay timeline.now


update : Time.Posix -> Timeline event -> Timeline event
update time tl =
    updateWith True time tl


{-| -}
updateWith : Bool -> Time.Posix -> Timeline event -> Timeline event
updateWith withGC possiblyNow (Timeline timeline) =
    let
        -- we can only move forward with updating
        -- This is so that the Animator event "GC" doesn't cause awkward skips.
        -- NOTE: for something like debug mode, we might want to disable this
        -- to allow scrubbing a timeline.
        now =
            Quantity.max (Time.absolute possiblyNow) timeline.now
    in
    { timeline | now = now }
        |> applyQueued
        |> applyInterruptions
        |> clean withGC
        |> Timeline


{-| Garbage collect and update `isRunning`
-}
clean : Bool -> TimelineDetails event -> TimelineDetails event
clean runGC details =
    let
        running =
            case details.events of
                Timetable lines ->
                    linesAreActive details.now lines

        retained =
            if runGC then
                collectHistory details

            else
                details
    in
    { retained | running = running }


gc : Timeline event -> Timeline event
gc (Timeline details) =
    Timeline (collectHistory details)


{-| View functions may request any delay up to this duration after `update` has
already run. Collection must preserve the entire supported lookback window.
-}
maxDelay : Time.Duration
maxDelay =
    Duration.seconds 5


type alias Retained event =
    { initial : event, events : Timetable event }


collectHistory : TimelineDetails event -> TimelineDetails event
collectHistory details =
    let
        oldestSample =
            Time.rollbackBy maxDelay details.now

        retained =
            case details.events of
                Timetable lines ->
                    findAnchor oldestSample
                        (Time.millis 0)
                        Nothing
                        []
                        lines
                        details.initial
                        { initial = details.initial, events = details.events }
    in
    if retained.events == details.events && retained.initial == details.initial then
        details

    else
        { details
            | initial = retained.initial
            , events = retained.events

            -- Regenerate CSS relative to now, preserving phase with delays.
            , updatedAt = details.now
        }


{-| Only a reached state can anchor retained history: an unfinished interruption
chain still needs its earlier motions. Keep the actual arrival/dwell times for
resting sequences and the preceding reached state for `previous`.
-}
findAnchor :
    Time.Absolute
    -> Time.Absolute
    -> Maybe Time.Absolute
    -> List (Occurring event)
    -> List (Line event)
    -> event
    -> Retained event
    -> Retained event
findAnchor oldest start cutoff queue future lastArrived retained =
    case queue of
        [] ->
            case future of
                [] ->
                    retained

                (Line lineStart first rest) :: following ->
                    findAnchor oldest
                        lineStart
                        (List.head following |> Maybe.map lineStartTime)
                        (first :: rest)
                        following
                        lastArrived
                        retained

        event :: rest ->
            if Maybe.map (Time.thisAfterThat start) cutoff |> Maybe.withDefault False then
                findAnchor oldest start cutoff [] future lastArrived retained

            else if Time.thisAfterThat start oldest then
                retained

            else
                let
                    arrival =
                        startTime event

                    reached =
                        Time.thisBeforeOrEqualThat arrival oldest
                            && (Maybe.map (Time.thisBeforeOrEqualThat arrival) cutoff |> Maybe.withDefault True)
                in
                if reached then
                    findAnchor oldest
                        (endTime event)
                        cutoff
                        rest
                        future
                        (getEvent event)
                        { initial = lastArrived
                        , events = Timetable (Line arrival event rest :: future)
                        }

                else
                    findAnchor oldest (endTime event) cutoff rest future lastArrived retained


lineStartTime : Line event -> Time.Absolute
lineStartTime (Line start _ _) =
    start


beforeLineEnd : Time.Absolute -> Line event -> Bool
beforeLineEnd time (Line lineStartAt startingEvent trailing) =
    if Time.thisBeforeOrEqualThat time lineStartAt then
        True

    else
        case trailing of
            [] ->
                Time.thisBeforeThat time (endTime startingEvent)

            _ ->
                beforeEventEnd time trailing


beforeEventEnd : Time.Absolute -> List (Occurring event) -> Bool
beforeEventEnd time events =
    case events of
        [] ->
            False

        top :: remain ->
            if Time.thisBeforeThat time (endTime top) then
                True

            else
                beforeEventEnd time remain


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
                    if timeline.scale == 1 then
                        enqueue timeline timeline.now queued

                    else
                        queued
                            |> scaleSchedule timeline.scale
                            |> enqueue timeline timeline.now
                , queued = Nothing
                , updatedAt = timeline.now
            }


{-|

    {-| A list of events that haven't been added to the schedule yet.

-}
type Schedule event
= Schedule Time.Duration (Event event) (List (Event event))

-}
scaleSchedule : Float -> Schedule event -> Schedule event
scaleSchedule scale (Schedule dur event events) =
    Schedule (Time.scaleDuration scale dur)
        (scaleEvent scale event)
        (List.map (scaleEvent scale) events)


scaleEvent : Float -> Event event -> Event event
scaleEvent scale (Event dur event maybeDur) =
    Event (Time.scaleDuration scale dur)
        event
        (Maybe.map (Time.scaleDuration scale) maybeDur)


{-|

    *NOTE* - this only looks at the most immediately upcoming event and does an equality check.
    There may be other cases we want to cover here, though this is the most common one by far.
    However, others could be captured by manually checking if events are `Timeline.upcoming`

-}
scheduleMatchesExisting : Schedule event -> Line event -> Bool
scheduleMatchesExisting (Schedule _ event schedulUpcoming) (Line _ lineStartEvent lineUpcoming) =
    let
        equalStartEvent =
            scheduledEventEqual event lineStartEvent

        equalUpcoming =
            case schedulUpcoming of
                [] ->
                    case lineUpcoming of
                        [] ->
                            True

                        _ ->
                            False

                _ ->
                    False
    in
    equalStartEvent && equalUpcoming


scheduledEventEqual : Event event -> Occurring event -> Bool
scheduledEventEqual (Event _ schedEvent _) (Occurring occurEvent _ _) =
    schedEvent == occurEvent



{- INTERRUPTION -}


applyInterruptions : TimelineDetails event -> TimelineDetails event
applyInterruptions timeline =
    -- Note, the foldl is reversing the interruptions, which is intentional
    -- we reverse the interruptions so that they're applied as First-in-First-Out.
    -- If we do Last-in-First-Out we run into issues.
    -- Imagine mouse events coming in where there is movement and then an end.
    -- It means `timeline.interruptions` would be the following
    -- [End, Move, Move, Move]
    -- We have to reverse the list so they're processed as [Move, Move, Move, End]
    let
        discountInterruption schedule discounted =
            -- If we're returning to a previous state while enroute to a new state,
            -- we can "discount" the duration to return.
            if Duration.isZero (scheduleDelay schedule) && previous (Timeline timeline) == currentScheduleTarget schedule then
                let
                    maxProgress =
                        transitionProgress (Timeline timeline)
                            |> List.maximum
                            |> Maybe.withDefault 1
                in
                (schedule
                    |> scaleScheduleDurationBy maxProgress
                )
                    :: discounted

            else
                schedule :: discounted
    in
    case List.foldl discountInterruption [] timeline.interruption of
        [] ->
            timeline

        interruptions ->
            applyInterruptionHelper interruptions
                { timeline
                    | interruption = []
                    , updatedAt = timeline.now
                }


applyInterruptionHelper : List (Schedule event) -> TimelineDetails event -> TimelineDetails event
applyInterruptionHelper interrupts timeline =
    case interrupts of
        [] ->
            timeline

        inter :: remaining ->
            applyInterruptionHelper remaining
                { timeline
                    | events =
                        if timeline.scale == 1 then
                            interrupt timeline
                                inter

                        else
                            interrupt timeline
                                (scaleSchedule timeline.scale inter)
                }


scaleScheduleDurationBy : Float -> Schedule state -> Schedule state
scaleScheduleDurationBy factor (Schedule currentScheduleDelay (Event dur checkpoint dwell) events) =
    Schedule
        currentScheduleDelay
        (Event (Duration.scale factor dur) checkpoint dwell)
        events


{-| Interrupt a current timetable with a new list of events.

    - If this timeline is after all other timelines
        -> queue it to the end and extend the dwell of the last event
    - otherwise, add as a new `Line` to the timetable.

-}
interrupt : TimelineDetails events -> Schedule events -> Timetable events
interrupt timeline scheduled =
    case timeline.events of
        Timetable lines ->
            case interruptLines timeline.now scheduled [] lines of
                Nothing ->
                    enqueue timeline timeline.now scheduled

                Just interrupted ->
                    Timetable interrupted


interruptLines : Time.Absolute -> Schedule event -> List (Line event) -> List (Line event) -> Maybe (List (Line event))
interruptLines now scheduled pastLines lines =
    case lines of
        [] ->
            Nothing

        startLine :: remaining ->
            let
                startInterruption =
                    Time.advanceBy (scheduleDelay scheduled) now
            in
            if interruptionHappensLater startInterruption remaining then
                interruptLines now scheduled (startLine :: pastLines) remaining

            else
                case interruptLine now scheduled startLine remaining of
                    Nothing ->
                        interruptLines now scheduled (startLine :: pastLines) remaining

                    Just interruption ->
                        if scheduleMatchesExisting scheduled startLine then
                            -- we're already enroute to this series of events, don't start it over.
                            Just (List.reverse pastLines ++ lines)

                        else if startInterruption == lineStartTime startLine && Time.thisAfterThat startInterruption now then
                            -- if the starting times are the same
                            -- then this new line replaces the current one.
                            Just (List.reverse pastLines ++ interruption)

                        else
                            -- interruption is the interruption in the proper order, embedded with remaining
                            Just (List.reverse pastLines ++ (startLine :: interruption))


interruptionHappensLater : Time.Absolute -> List (Line event) -> Bool
interruptionHappensLater startInterruption remaining =
    case remaining of
        [] ->
            False

        top :: _ ->
            Time.thisAfterOrEqualThat startInterruption (lineStartTime top)


interruptLine : Time.Absolute -> Schedule event -> Line event -> List (Line event) -> Maybe (List (Line event))
interruptLine now scheduled line future =
    case line of
        Line start startEvent trailing ->
            let
                startInterruption =
                    Time.advanceBy (scheduleDelay scheduled) now
            in
            if Time.thisAfterOrEqualThat startInterruption start then
                -- this line starts before the interruption
                case future of
                    [] ->
                        if beforeLineEnd startInterruption line then
                            Just
                                [ createLine now scheduled
                                ]

                        else
                            -- we'll just queue up this new line instead
                            Nothing

                    (Line nextStart next nextEvents) :: futureRemaining ->
                        -- we need to find the target event we're currently enroute to.
                        -- if the next line has already started, but the event hasnt happened yet
                        -- then we know `next` is the target
                        if
                            Time.thisAfterOrEqualThat startInterruption nextStart
                                && Time.thisBeforeOrEqualThat startInterruption (startTime next)
                        then
                            Just
                                (Line nextStart next nextEvents
                                    :: createLine now scheduled
                                    :: futureRemaining
                                )

                        else
                            Nothing

            else
                Nothing


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


{-| There's some nuance to when we can add events to a `Line`.

When interpolating we allow the interpolator to look ahead one event in order to calculate the desired velocity it should be at.

This lookahead only happens within Lines, which means we can only append to the current line if appending it would be after the event that we're using fro that calculation.

e.g.

        a------------b---------c-------d
            ^ now    ^---------^ these two events are used to calculate the desired velocity

So, if we have the above situation, then we could append to this line.

However, the below situation, we shouldnt.

        a-----------b---------c-------d
                                  ^ now

**However!** **Both queueing and interruptions should create a new \`Line**

    - This is to ensure that there is not retroactive effect.
    - Also!  If we're conditionally changing a `Line` via queueing, it means the animation will be different depending on the timing of when the queueing happens! Oof. What if the player in a game is mashing buttons and animations change intermittently? No Bueno.

-}
addToCurrentLine : Time.Absolute -> Schedule event -> List (Line event) -> List (Line event)
addToCurrentLine now scheduled lines =
    case lines of
        [] ->
            [ createLine now scheduled ]

        line :: [] ->
            -- if we've gotten here, this line is current
            addEventsToLine now scheduled line []

        (Line startOne startEventOne one) :: (Line startTwo startEventTwo two) :: remaining ->
            -- we check if now is after startOne, but before startTwo
            if Time.thisAfterOrEqualThat now startOne && Time.thisBeforeThat now startTwo then
                -- one is the current timeline
                addEventsToLine now
                    scheduled
                    (Line startOne startEventOne one)
                    (Line startTwo startEventTwo two
                        :: remaining
                    )

            else
                -- need to search farther.
                Line startOne startEventOne one
                    :: addToCurrentLine now scheduled (Line startTwo startEventTwo two :: remaining)


createLine : Time.Absolute -> Schedule events -> Line events
createLine now (Schedule delay (Event dur startEvent maybeDwell) reverseQueued) =
    let
        start =
            now
                |> Time.advanceBy dur
                |> Time.advanceBy delay

        startNextEvent =
            case maybeDwell of
                Nothing ->
                    start

                Just dwell ->
                    Time.advanceBy dwell start

        events =
            List.reverse reverseQueued
                |> List.foldl toOccurring ( startNextEvent, [] )
                |> Tuple.second
                |> List.reverse
    in
    Line
        (Time.advanceBy delay now)
        -- now
        (Occurring startEvent start startNextEvent)
        events


{-| Given our explanation above, this function does the following

    1. modifies the last event of the existing line as necessary
    2. creates a new line representing the queueing.

-}
addEventsToLine : Time.Absolute -> Schedule events -> Line events -> List (Line events) -> List (Line events)
addEventsToLine now scheduled (Line startLineAt startingEvent events) lines =
    case List.reverse events of
        [] ->
            let
                startNewEventsAt =
                    Time.latest
                        (endTime startingEvent)
                        now

                newLine =
                    createLine startNewEventsAt scheduled

                startingEventWithDwell =
                    case startingEvent of
                        Occurring ev eventStart _ ->
                            -- if the scheduled events are way after the current event
                            -- extend that events dwell until the start of the scheduled stuff
                            Occurring ev eventStart (Time.advanceBy (scheduleDelay scheduled) startNewEventsAt)
            in
            Line startLineAt startingEventWithDwell [] :: newLine :: lines

        (Occurring lastEvent lastEventTime lastEventFinish) :: eventTail ->
            let
                startNewEventsAt =
                    Time.latest
                        lastEventFinish
                        now

                newLine =
                    createLine startNewEventsAt scheduled

                -- we need to increase the dwell time of the last event
                -- to match the start time of the new queued events.
                newLastEvent =
                    Occurring lastEvent
                        lastEventTime
                        -- createLine handles applying the schedule scheduleDelay
                        -- but we need to apply it here manually
                        (Time.advanceBy (scheduleDelay scheduled) startNewEventsAt)
            in
            Line startLineAt
                startingEvent
                (List.reverse (newLastEvent :: eventTail))
                :: newLine
                :: lines


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


foldpAll :
    Time.Absolute
    -> (state -> anchor)
    -> (anchor -> motion)
    -> Transition state anchor motion
    -> Timeline state
    -> motion
foldpAll now lookup toStart transitionTo (Timeline timelineDetails) =
    case timelineDetails.events of
        Timetable timetable ->
            visitAll now
                lookup
                transitionTo
                (Time.millis 0)
                Nothing
                []
                timetable
                (toStart (lookup timelineDetails.initial))


{-| -}
visitAll :
    Time.Absolute
    -> (state -> anchor)
    -> Transition state anchor motion
    -> Time.Absolute
    -> Maybe Time.Absolute
    -> List (Occurring state)
    -> List (Line state)
    -> motion
    -> motion
visitAll now toAnchor transitionTo start cutoff queue future state =
    -- Visit each reachable transition exactly once. The next line truncates
    -- this line, including any queued events which never get to start.
    case queue of
        [] ->
            case future of
                [] ->
                    state

                (Line lineStart first rest) :: following ->
                    let
                        nextCutoff =
                            case following of
                                (Line nextStart _ _) :: _ ->
                                    Just nextStart

                                [] ->
                                    Nothing
                    in
                    visitAll now toAnchor transitionTo lineStart nextCutoff (first :: rest) following state

        top :: remain ->
            if Maybe.map (Time.thisAfterThat start) cutoff |> Maybe.withDefault False then
                visitAll now toAnchor transitionTo start cutoff [] future state

            else
                let
                    arrival =
                        startTime top

                    end =
                        case cutoff of
                            Just interrupted ->
                                if Time.thisBeforeThat interrupted arrival then
                                    interrupted

                                else
                                    arrival

                            Nothing ->
                                arrival

                    new =
                        transitionTo toAnchor top now start end remain state
                in
                visitAll now toAnchor transitionTo (endTime top) cutoff remain future new



{- BOOKKEEPING -}


type Status
    = Dwelling Time.Duration
    | Transitioning
        { progress : Float
        , transitionProgress : List Float
        }


status : Timeline event -> Status
status timeline =
    foldpAll (getCurrentTime timeline)
        identity
        (\_ -> Dwelling Time.zeroDuration)
        (\_ target now start end _ found ->
            let
                startTimeTarget =
                    startTime target

                sampledAt =
                    if Time.thisBeforeThat end now then
                        end

                    else
                        now
            in
            if Time.thisBeforeThat now start then
                found

            else if Time.thisAfterOrEqualThat sampledAt startTimeTarget then
                Dwelling (Time.duration now startTimeTarget)

            else
                case found of
                    Transitioning trans ->
                        Transitioning
                            { progress =
                                Time.progress start startTimeTarget sampledAt
                            , transitionProgress =
                                trans.progress :: trans.transitionProgress
                            }

                    Dwelling _ ->
                        Transitioning
                            { progress =
                                Time.progress start startTimeTarget sampledAt
                            , transitionProgress = []
                            }
        )
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
            t.progress


transitionProgress : Timeline state -> List Float
transitionProgress timeline =
    case status timeline of
        Dwelling _ ->
            []

        Transitioning t ->
            t.progress :: t.transitionProgress


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


arrived : Timeline event -> event
arrived ((Timeline details) as timeline) =
    foldpAll (getCurrentTime timeline)
        identity
        (\_ -> details.initial)
        (\_ target now _ endTransition _ state ->
            -- Arrived value is the last value that we've successfully arrived at
            if
                Time.thisAfterOrEqualThat now endTransition
                    && (startTime target == endTransition)
            then
                getEvent target

            else
                state
        )
        timeline


current : Timeline event -> event
current ((Timeline details) as timeline) =
    foldpAll (getCurrentTime timeline)
        identity
        (\_ -> details.initial)
        (\_ target now start _ _ state ->
            if Time.thisAfterOrEqualThat now start then
                getEvent target

            else
                state
        )
        timeline


{-|

```ascii
                       Starting transitioning to C
                    |  |
          A---------B--B-------C
               ^    ^ ^  ^
previous:      A    A A  B
```

-}
previous : Timeline event -> event
previous ((Timeline details) as timeline) =
    foldpAll (getCurrentTime timeline)
        identity
        (\_ -> ( details.initial, details.initial ))
        (\_ target now start endTransition _ (( _, lastArrived ) as state) ->
            if startTime target == endTransition && Time.thisAfterOrEqualThat now endTransition then
                ( lastArrived, getEvent target )

            else if Time.thisAfterThat now start then
                ( lastArrived, lastArrived )

            else
                state
        )
        timeline
        |> Tuple.first


arrivedAt : (event -> Bool) -> Time.Posix -> Timeline event -> Bool
arrivedAt matches newTime ((Timeline details) as tl) =
    foldpAll (getCurrentTime tl)
        identity
        (\_ -> False)
        (\_ target now _ end _ state ->
            state
                || (matches (getEvent target)
                        && startTime target
                        == end
                        && Time.thisBeforeThat now end
                        && Time.thisAfterOrEqualThat (Time.rollbackBy details.delay (Time.absolute newTime)) end
                   )
        )
        tl


onMaybe : (a -> Bool) -> Maybe a -> Bool
onMaybe fn maybe =
    case maybe of
        Nothing ->
            False

        Just thing ->
            fn thing


matchesEvent : (event -> Bool) -> Event event -> Bool
matchesEvent matches (Event _ event _) =
    matches event


anyScheduled : (event -> Bool) -> Schedule event -> Bool
anyScheduled matches (Schedule _ startEvent remainingEvents) =
    if matchesEvent matches startEvent then
        True

    else
        List.any (matchesEvent matches) remainingEvents


{-| -}
upcoming : (event -> Bool) -> Timeline event -> Bool
upcoming matches ((Timeline details) as tl) =
    -- we check both the queued and interruption caches
    -- This function is sometimes used to prevent queueing an action multiple times
    -- However if multiple msgs get fired in one frame, then there's still a subtle possibility that something will get double queued.
    if onMaybe (anyScheduled matches) details.queued then
        True

    else if List.any (anyScheduled matches) details.interruption then
        True

    else
        foldpAll (getCurrentTime tl)
            identity
            (\_ -> False)
            (\_ target now _ end _ state ->
                state
                    || (matches (getEvent target)
                            && startTime target
                            == end
                            && Time.thisBeforeThat now end
                       )
            )
            tl
