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
    , gc, atTime, dwellingTime, getCurrentTime, linesAreActive
    , Transition
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

@docs gc, atTime, dwellingTime, getCurrentTime, linesAreActive

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
    , delay : Time.Duration
    , scale : Float
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


type alias Transition state anchor motion =
    (state -> anchor)
    -- previous event
    -> Occurring state
    -- target event
    -> Occurring state
    -- now
    -> Time.Absolute
    -- start time:
    -- Either the end time of the previous transition
    -- or when the interruption happened
    -> Time.Absolute
    -- end time:
    -- This is either the endtime of `target event`
    -- or the interruption time
    -> Time.Absolute
    -- the future, but we can only look 1 deep
    -- this should be a maybe, but dont want to allocate it.
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


getCurrentTime : Timeline event -> Time.Absolute
getCurrentTime (Timeline timeline) =
    Time.rollbackBy timeline.delay timeline.now


cutoff : Time.Absolute
cutoff =
    --Time.millis 1618490321242
    Time.millis 0


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
                |> Quantity.minus cutoff
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
    in
    { details
        | running =
            running
        , events =
            if runGC then
                let
                    events =
                        case details.events of
                            Timetable evs ->
                                evs
                in
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
            if Time.thisAfterThat startAt now then
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
                                Time.thisAfterThat now interruptionTime
                in
                if interrupted then
                    garbageCollectOldEvents now (Line startAt startingEvent events :: droppable) remaining

                else
                    case hewLine now (startingEvent :: events) of
                        NothingCaptured ->
                            List.reverse droppable ++ lines

                        Captured capturedLine ->
                            capturedLine :: remaining


type HewStatus event
    = Captured (Line event)
    | NothingCaptured


hewLine : Time.Absolute -> List (Occurring event) -> HewStatus event
hewLine now events =
    hewlineHelper now Nothing events


hewlineHelper : Time.Absolute -> Maybe (Occurring event) -> List (Occurring event) -> HewStatus event
hewlineHelper now maybePrevious events =
    case events of
        [] ->
            NothingCaptured

        top :: remaining ->
            if dwellingAt now top then
                case maybePrevious of
                    Nothing ->
                        Captured (Line (startTime top) top remaining)

                    Just prev ->
                        Captured (Line (startTime prev) prev (top :: remaining))

            else if Time.thisAfterThat now (endTime top) then
                hewlineHelper now (Just top) remaining

            else
                NothingCaptured


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
                    if timeline.scale == 1 then
                        enqueue timeline timeline.now queued

                    else
                        queued
                            |> scaleSchedule timeline.scale
                            |> enqueue timeline timeline.now
                , queued = Nothing
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
    case timeline.interruption of
        [] ->
            timeline

        _ ->
            -- Note, we reverse the interruptions so that they're applied as First-in-First-Out.
            -- If we do Last-in-First-Out we run into issues.
            -- Imagine mouse events coming in where there is movement and then an end.
            -- It means `timeline.interruptions` would be the following
            -- [End, Move, Move, Move]
            -- We have to reverse the list so they're processed as [Move, Move, Move, End]
            applyInterruptionHelper (List.reverse timeline.interruption)
                { timeline | interruption = [] }


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
                    case scheduled of
                        Schedule scheduleDelay _ _ ->
                            Time.advanceBy scheduleDelay now
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
                    case scheduled of
                        Schedule scheduleDelay _ _ ->
                            Time.advanceBy scheduleDelay now
            in
            if Time.thisAfterOrEqualThat startInterruption start then
                -- this line starts before the interruption
                case future of
                    [] ->
                        case getTransitionAt startInterruption startEvent trailing of
                            Nothing ->
                                if beforeLineEnd startInterruption line then
                                    Just
                                        [ createLine now scheduled
                                        ]

                                else
                                    -- we'll just queue up this new line instead
                                    Nothing

                            Just last2Events ->
                                Just
                                    [ interruptAtExactly now scheduled last2Events ]

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
                                    :: interruptAtExactly now
                                        scheduled
                                        (LastTwoEvents
                                            (endTime startEvent)
                                            (getEvent startEvent)
                                            (startTime next)
                                            (getEvent next)
                                        )
                                    :: futureRemaining
                                )

                        else
                            Nothing

            else
                Nothing


{-| This will provide the two events we are currently between.
-}
getTransitionAt : Time.Absolute -> Occurring event -> List (Occurring event) -> Maybe (LastTwoEvents event)
getTransitionAt interruptionTime prev trailing =
    case trailing of
        [] ->
            Nothing

        next :: remain ->
            if Time.thisAfterOrEqualThat interruptionTime (endTime prev) && Time.thisBeforeThat interruptionTime (startTime next) then
                Just (LastTwoEvents (endTime prev) (getEvent prev) (startTime next) (getEvent next))

            else
                getTransitionAt interruptionTime next remain


interruptAtExactly : Time.Absolute -> Schedule event -> LastTwoEvents event -> Line event
interruptAtExactly now scheduled (LastTwoEvents penultimateTime penultimate lastEventTime _) =
    case scheduled of
        Schedule delay_ startingEvent reverseQueued ->
            let
                newStartingEvent =
                    -- we apply the discount if we are returning to a state
                    if penultimate == getScheduledEvent startingEvent then
                        let
                            amountProgress =
                                Time.progress penultimateTime
                                    lastEventTime
                                    (Time.advanceBy delay_ now)
                        in
                        startingEvent
                            |> adjustScheduledDuration (Quantity.multiplyBy amountProgress)

                    else
                        startingEvent
            in
            createLine now
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
addEventsToLine now ((Schedule scheduleDelay _ _) as scheduled) (Line startLineAt startingEvent events) lines =
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
                            Occurring ev eventStart (Time.advanceBy scheduleDelay startNewEventsAt)
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
                        (Time.advanceBy scheduleDelay startNewEventsAt)
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
    (state -> anchor)
    -> (anchor -> motion)
    -> Transition state anchor motion
    -> Timeline state
    -> motion
foldpAll lookup toStart transitionTo (Timeline timelineDetails) =
    case timelineDetails.events of
        Timetable timetable ->
            let
                start =
                    toStart (lookup timelineDetails.initial)
            in
            case timetable of
                [] ->
                    start

                (Line lineStart _ _) :: _ ->
                    visitAll2
                        lookup
                        transitionTo
                        timelineDetails
                        (Occurring timelineDetails.initial lineStart lineStart)
                        []
                        timetable
                        start


{-| -}
visitAll2 :
    (state -> anchor)
    -> Transition state anchor motion
    -> TimelineDetails state
    -> Occurring state
    -> List (Occurring state)
    -> List (Line state)
    -> motion
    -> motion
visitAll2 toAnchor transitionTo details prev queue future state =
    -- queue: the upcoming events on this Line
    case queue of
        [] ->
            case future of
                [] ->
                    state

                (Line futureStart futureEvent futureRemain) :: [] ->
                    -- the last line.
                    -- transition to futureEvent and then continue on through the remaining events in futureRemain
                    let
                        new =
                            state
                                |> transitionTo
                                    toAnchor
                                    prev
                                    futureEvent
                                    details.now
                                    futureStart
                                    (startTime futureEvent)
                                    futureRemain
                    in
                    visitAll2
                        toAnchor
                        transitionTo
                        details
                        futureEvent
                        futureRemain
                        []
                        new

                (Line futureStart futureEvent futureRemain) :: (((Line nextStart nextEvent nextRemain) :: restOfFuture) as allFuture) ->
                    if Time.thisBeforeThat nextStart (endTime futureEvent) then
                        -- we've been interrupted!
                        let
                            new =
                                state
                                    |> transitionTo
                                        toAnchor
                                        prev
                                        futureEvent
                                        details.now
                                        --v transition start time
                                        futureStart
                                        --v transition end time
                                        nextStart
                                        futureRemain
                                    |> transitionTo toAnchor
                                        prev
                                        nextEvent
                                        details.now
                                        nextStart
                                        (endTime nextEvent)
                                        nextRemain
                        in
                        visitAll2
                            toAnchor
                            transitionTo
                            details
                            nextEvent
                            nextRemain
                            restOfFuture
                            new

                    else
                        let
                            new =
                                state
                                    |> transitionTo
                                        toAnchor
                                        prev
                                        futureEvent
                                        details.now
                                        (endTime prev)
                                        (endTime futureEvent)
                                        futureRemain
                        in
                        visitAll2
                            toAnchor
                            transitionTo
                            details
                            futureEvent
                            futureRemain
                            allFuture
                            new

        top :: remain ->
            case future of
                [] ->
                    let
                        new =
                            transitionTo toAnchor
                                prev
                                top
                                details.now
                                (endTime prev)
                                (startTime top)
                                remain
                                state
                    in
                    visitAll2
                        toAnchor
                        transitionTo
                        details
                        top
                        remain
                        future
                        new

                (Line futureStart futureEvent futureRemain) :: restOfFuture ->
                    if Time.thisBeforeThat futureStart (endTime top) then
                        -- enroute to `top`, we are interrupted
                        -- so we transition to top (stopping at the interruption point)
                        -- then make another transition from where we were interrupted to
                        -- our new destination
                        let
                            new =
                                state
                                    |> transitionTo toAnchor
                                        prev
                                        top
                                        details.now
                                        (endTime prev)
                                        futureStart
                                        remain
                                    |> transitionTo toAnchor
                                        prev
                                        futureEvent
                                        details.now
                                        futureStart
                                        (endTime futureEvent)
                                        futureRemain
                        in
                        visitAll2
                            toAnchor
                            transitionTo
                            details
                            futureEvent
                            futureRemain
                            restOfFuture
                            new

                    else
                        let
                            new =
                                transitionTo toAnchor
                                    prev
                                    top
                                    details.now
                                    (endTime prev)
                                    (endTime top)
                                    remain
                                    state
                        in
                        visitAll2
                            toAnchor
                            transitionTo
                            details
                            top
                            remain
                            future
                            new



{- BOOKKEEPING -}


type Status
    = Dwelling Time.Duration
    | Transitioning Float


status : Timeline event -> Status
status timeline =
    foldpAll identity
        (\_ -> Dwelling Time.zeroDuration)
        (\_ _ _ now start end _ _ ->
            if Time.thisAfterThat now end then
                Dwelling (Time.duration now end)

            else
                Transitioning (Time.progress start end now)
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


arrived : Timeline event -> event
arrived ((Timeline details) as timeline) =
    foldpAll identity
        (\_ -> details.initial)
        (\_ _ target now _ end _ state ->
            -- This is the current event when
            --      we have started toward an event or arrived at it.
            -- A tricky aspect is that css timelines are only updated on transition
            -- This means that now == start must be current, or else current will be wrong for the whole transition.
            if Time.thisAfterOrEqualThat now end && (startTime target == end) then
                getEvent target

            else
                state
        )
        timeline


current : Timeline event -> event
current ((Timeline details) as timeline) =
    foldpAll identity
        (\_ -> details.initial)
        (\_ _ target now start end future state ->
            -- This is the current event when
            --      we have started toward an event or arrived at it.
            -- A tricky aspect is that css timelines are only updated on transition
            -- This means that now == start must be current, or else current will be wrong for the whole transition.
            if
                Time.thisBeforeOrEqualThat now end
                    && Time.thisAfterOrEqualThat now start
            then
                getEvent target

            else if List.isEmpty future && Time.thisAfterThat now end then
                getEvent target

            else
                state
        )
        timeline


previous : Timeline event -> event
previous ((Timeline details) as timeline) =
    foldpAll identity
        (\_ -> details.initial)
        (\_ _ target now _ _ future state ->
            if Time.thisAfterThat now (endTime target) then
                case future of
                    [] ->
                        state

                    _ ->
                        getEvent target

            else
                state
        )
        timeline


arrivedAt : (event -> Bool) -> Time.Posix -> Timeline event -> Bool
arrivedAt matches newTime ((Timeline details) as tl) =
    foldpAll identity
        (\_ -> False)
        (\_ _ target _ _ end _ state ->
            state
                || (matches (getEvent target)
                        && Time.thisBeforeOrEqualThat details.now end
                        && Time.thisAfterOrEqualThat (Time.absolute newTime) end
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
        foldpAll identity
            (\_ -> False)
            (\_ _ target now _ end _ state ->
                state
                    || (matches (getEvent target)
                            && Time.thisBeforeThat now end
                       )
            )
            tl
