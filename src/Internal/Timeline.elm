module Internal.Timeline exposing
    ( Timeline(..), TimelineDetails, Occurring(..), getEvents
    , Schedule(..), Event(..)
    , update, needsUpdate
    , startTime, endTime, getEvent, extendEventDwell, hasDwell
    , addToDwell
    , progress, dwellingTime
    , current, arrivedAt, arrived, previous, upcoming
    , Line(..), Timetable(..)
    , foldp, capture, captureTimeline
    , ActualDuration(..), Animator(..), Description(..), Frame(..), Frames(..), FramesSummary, Interp, LookAhead, Period(..), Previous(..), Resting(..), Summary, SummaryEvent(..), atTime, foldpAll, foldpOld, gc, getCurrentTime, hasChanged, isDuring, isOnce, justInitialized, linearDefault, linesAreActive, mapLookAhead, periodDuration, previousEndTime, previousStartTime, reduceIterations, updateWith
    )

{-|

@docs Timeline, TimelineDetails, Occurring, getEvents

@docs Schedule, Event

@docs update, needsUpdate

@docs startTime, endTime, getEvent, extendEventDwell, hasDwell

@docs addToDwell

@docs progress, dwellingTime

@docs current, arrivedAt, arrived, previous, upcoming

@docs Line, Timetable

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


reduceIterations : Int -> Period -> Period
reduceIterations removeN period =
    case period of
        Loop _ ->
            period

        Repeat n dur ->
            Repeat (n - removeN) dur


isDuring : Time.Absolute -> Time.Absolute -> Period -> Bool
isDuring now start period =
    case period of
        Loop _ ->
            Time.thisAfterOrEqualThat now start

        Repeat n dur ->
            let
                end =
                    start
                        |> Time.advanceBy
                            (Quantity.multiplyBy (toFloat n) dur)
            in
            Time.thisAfterOrEqualThat now start
                && Time.thisBeforeOrEqualThat now end


isOnce : Period -> Bool
isOnce per =
    case per of
        Loop dur ->
            False

        Repeat i _ ->
            (i - 0) == 1


periodDuration : Period -> Time.Duration
periodDuration per =
    case per of
        Loop dur ->
            dur

        Repeat i dur ->
            dur


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


{-| First, let's cover what the type parameters are

Examples:

    The symbolic state of the timeline entry.
    state -> MenuOpen

    The description of how to animate that symbolic state
    anchor ->
        = Osc Personality Period (Float -> Float)
        | Pos Personality Float

    The actual value that's moving
    motion -> { x:34, y: 34 }

-}
type alias Interp state anchor motion =
    { start : anchor -> motion
    , adjustor : GetPersonality anchor
    , dwellPeriod : DwellPeriod anchor
    , visit : Visit state anchor motion
    , lerp : Lerp anchor motion
    }


type alias Visit state anchor motion =
    (state -> anchor)
    -> Occurring state
    -> Time.Absolute
    -> Maybe (LookAhead anchor)
    -> motion
    -> motion


type alias LookAhead state =
    { anchor : state
    , time : Time.Absolute
    , resting : Bool
    }


mapLookAhead : (a -> b) -> LookAhead a -> LookAhead b
mapLookAhead fn look =
    { anchor = fn look.anchor
    , time = look.time
    , resting = look.resting
    }


type alias Lerp anchor motion =
    Time.Absolute
    -> anchor
    -> anchor
    -> Time.Absolute
    -> Time.Absolute
    -> Maybe (LookAhead anchor)
    -> motion
    -> motion


type alias DwellPeriod anchor =
    anchor -> Maybe Period


type Previous event
    = Previous (Occurring event)
    | PreviouslyInterrupted Time.Absolute



-- {-| -}
-- type Pause
--     = Pause Time.Duration Float


type alias GetPersonality anchor =
    anchor -> Personality


type alias Personality =
    { wobbliness : Float
    , impulse : Float
    , arriveEarly : Float
    , arriveSlowly : Float
    , departLate : Float
    , departSlowly : Float
    }


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


adjustTime : (event -> anchor) -> GetPersonality anchor -> Occurring event -> List (Occurring event) -> Occurring event
adjustTime lookup getPersonality ((Occurring event start eventEnd) as unmodified) upcomingOccurring =
    case upcomingOccurring of
        [] ->
            unmodified

        (Occurring next nextStartTime _) :: _ ->
            let
                personality =
                    getPersonality (lookup event)
            in
            if personality.departLate /= 0 then
                let
                    totalDuration =
                        Time.duration eventEnd nextStartTime

                    nextPersonality =
                        getPersonality (lookup next)

                    -- if portions sum to more than 1, then that sum represents the full duration
                    totalPortions =
                        max
                            (personality.departLate + nextPersonality.arriveEarly)
                            1

                    lateBy =
                        Quantity.multiplyBy
                            (personality.departLate / totalPortions)
                            totalDuration
                in
                Occurring event start (Time.advanceBy lateBy eventEnd)

            else
                unmodified


adjustTimeWithPrevious : (event -> anchor) -> GetPersonality anchor -> Occurring event -> Occurring event -> List (Occurring event) -> Occurring event
adjustTimeWithPrevious lookup getPersonality (Occurring prev prevStart prevEnd) ((Occurring event start eventEnd) as unmodified) upcomingOccurring =
    let
        personality =
            getPersonality (lookup event)

        prevPersonality =
            getPersonality (lookup prev)

        totalPrevDuration =
            Time.duration prevEnd start

        -- if portions sum to more than 1, then that sum represents the full duration
        totalPrevPortions =
            max
                (prevPersonality.departLate + personality.arriveEarly)
                1

        earlyBy =
            Quantity.multiplyBy
                (personality.arriveEarly / totalPrevPortions)
                totalPrevDuration
    in
    case upcomingOccurring of
        [] ->
            if Time.zeroDuration earlyBy then
                unmodified

            else
                Occurring event (Time.rollbackBy earlyBy start) eventEnd

        (Occurring next nextStartTime _) :: _ ->
            if personality.departLate /= 0 then
                let
                    totalDuration =
                        Time.duration eventEnd nextStartTime

                    nextPersonality =
                        getPersonality (lookup next)

                    -- if portions sum to more than 1, then that sum represents the full duration
                    totalPortions =
                        max
                            (personality.departLate + nextPersonality.arriveEarly)
                            1

                    lateBy =
                        Quantity.multiplyBy
                            (personality.departLate / totalPortions)
                            totalDuration
                in
                Occurring event (Time.rollbackBy earlyBy start) (Time.advanceBy lateBy eventEnd)

            else if Time.zeroDuration earlyBy then
                unmodified

            else
                Occurring event (Time.rollbackBy earlyBy start) eventEnd


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


getCurrentTime : Timeline event -> Time.Absolute
getCurrentTime (Timeline timeline) =
    Time.rollbackBy timeline.delay timeline.now


update : Time.Posix -> Timeline event -> Timeline event
update =
    updateWith True


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
            |> clean withGC
            |> Timeline

    else
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


reverseEvents : Line event -> Line event
reverseEvents (Line start event evs) =
    Line start event (List.reverse evs)


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
            -- Note, we reverse the interruptions so that they're applied as First-in-First-Out.
            -- If we do Last-in-First-Out we run into issues.
            -- Imagine mouse events coming in where there is movement and then an end.
            -- It means `timeline.interruptions` would be the following
            -- [End, Move, Move, Move]
            -- We have to reverse the list so they're processed as [Move, Move, Move, End]
            applyInterruptionHelper (List.reverse timeline.interruption) { timeline | interruption = [] }


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


{-| This will provide the two events we are currently between.
-}
getTransitionAt interruptionTime prev trailing =
    case trailing of
        [] ->
            Nothing

        next :: remain ->
            if Time.thisAfterOrEqualThat interruptionTime (endTime prev) && Time.thisBeforeThat interruptionTime (startTime next) then
                Just (LastTwoEvents (endTime prev) (getEvent prev) (startTime next) (getEvent next))

            else
                getTransitionAt interruptionTime next remain


interruptAtExactly startInterruption scheduled ((LastTwoEvents penultimateTime penultimate lastEventTime lastEvent) as last) =
    case scheduled of
        Schedule delay_ startingEvent reverseQueued ->
            let
                amountProgress =
                    Time.progress penultimateTime lastEventTime startInterruption

                newStartingEvent =
                    -- we apply the discount if we are returning to a state
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
createLine now ((Schedule _ (Event dur startEvent maybeDwell) reverseQueued) as scheduled) =
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
            List.reverse reverseQueued
                |> List.foldl toOccurring ( startNextEvent, [] )
                |> Tuple.second
                |> List.reverse
    in
    Line now (Occurring startEvent start startNextEvent) events


{-| Given our explanation above, this function does the following

    1. modifies the last event of the existing line as necessary
    2. creates a new line representing the queueing.

-}
addEventsToLine : Time.Absolute -> Schedule events -> Line events -> List (Line events) -> List (Line events)
addEventsToLine now ((Schedule delay scheduledStartingEvent reverseQueued) as scheduled) ((Line startLineAt startingEvent events) as existing) lines =
    let
        start =
            Time.advanceBy delay now
    in
    case List.reverse events of
        [] ->
            let
                startNewEventsAt =
                    Time.latest (Time.advanceBy delay (endTime startingEvent)) start

                newLine =
                    createLine startNewEventsAt scheduled

                startingEventWithDwell =
                    case startingEvent of
                        Occurring ev lastEventTime _ ->
                            if Time.thisAfterThat start lastEventTime then
                                Occurring ev lastEventTime start

                            else
                                Occurring ev lastEventTime lastEventTime
            in
            Line startLineAt startingEventWithDwell [] :: newLine :: lines

        (Occurring lastEvent lastEventTime lastEventFinish) :: eventTail ->
            let
                startNewEventsAt =
                    Time.latest (Time.advanceBy delay lastEventFinish) start

                newLine =
                    createLine startNewEventsAt scheduled

                -- we need to increase the dwell time of the last event
                -- to match the start time of the new queued events.
                -- let
                newLastEvent =
                    Occurring lastEvent
                        lastEventTime
                        startNewEventsAt
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



-- {-| Focus allows you to focus on a specific piece of state.
-- Let's say you have a record with two fields
--     { one = 0
--     , two = -10
--     }
-- You being to transition `one` to 1.
--     { one = 0
--     , two = -10
--     }
--         --200ms-->
--         { one = 1
--         , two = -10
--         }
-- And while that's happening, you also schedule `two` to go to 0.
--     { one = 0 -> 1
--     , two = -10 -> 0
--     }
--         --x30ms->  (interrupted 30ms in)
--             { one = 1
--             , two = -10
--             }
--         --200ms->
--             { one = 1
--             , two = 0
--             }
-- Because we're watching the
-- The critical part of this is that
-- -}
-- focus : (state -> focus) -> Timeline state -> Timeline focus
-- focus toFocus (Timeline details) =
--     Debug.todo ""


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
foldpOld :
    (state -> anchor)
    -> Interp state anchor motion
    -> Timeline state
    -> motion
foldpOld lookup fn (Timeline timelineDetails) =
    log "OLD DONE" <|
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


foldp :
    (state -> anchor)
    -> Interp state anchor motion
    -> Timeline state
    -> motion
foldp lookup fn (Timeline timelineDetails) =
    -- log "NEW DONE" <|
    case timelineDetails.events of
        Timetable timetable ->
            let
                start =
                    fn.start (lookup timelineDetails.initial)

                now =
                    Time.rollbackBy timelineDetails.delay timelineDetails.now
            in
            case rescale now timelineDetails.scale timetable of
                [] ->
                    start

                (Line lineStart firstEvent remain) :: remainingLines ->
                    throughLines
                        True
                        now
                        lookup
                        fn
                        timelineDetails
                        (Occurring timelineDetails.initial lineStart lineStart)
                        []
                        timetable
                        start


rescale : Time.Absolute -> Float -> List (Line event) -> List (Line event)
rescale now scale lines =
    if scale == 1 then
        lines

    else
        List.map (rescaleLine now scale) lines


rescaleLine now scale (Line lineStart firstEvent remain) =
    Line
        (rescaleTime now scale lineStart)
        (rescaleEvent now scale firstEvent)
        (List.map (rescaleEvent now scale) remain)


rescaleTime : Time.Absolute -> Float -> Time.Absolute -> Time.Absolute
rescaleTime (Quantity.Quantity now) scale (Quantity.Quantity time) =
    Time.millis (now + ((time - now) * scale))


rescaleEvent : Time.Absolute -> Float -> Occurring event -> Occurring event
rescaleEvent now scale (Occurring event start end) =
    Occurring event (rescaleTime now scale start) (rescaleTime now scale end)


lookAhead :
    (event -> anchor)
    -> Occurring event
    ->
        { anchor : anchor
        , time : Time.Absolute
        , resting : Bool
        }
lookAhead lookup (Occurring ahead (Quantity.Quantity start) (Quantity.Quantity end)) =
    { anchor = lookup ahead
    , time = Quantity.Quantity start
    , resting =
        (start - end) /= 0
    }


foldpAll :
    (state -> anchor)
    -> Interp state anchor motion
    -> Timeline state
    -> motion
foldpAll lookup fn (Timeline timelineDetails) =
    case timelineDetails.events of
        Timetable timetable ->
            let
                start =
                    fn.start (lookup timelineDetails.initial)

                beginning =
                    case timetable of
                        [] ->
                            Occurring timelineDetails.initial timelineDetails.now timelineDetails.now

                        (Line lineStart firstEvent remain) :: _ ->
                            Occurring timelineDetails.initial lineStart lineStart
            in
            visitAll
                False
                lookup
                fn
                timelineDetails
                beginning
                []
                timetable
                start


{-| -}
visitAll :
    Bool
    -> (state -> anchor)
    -> Interp state anchor motion
    -> TimelineDetails state
    -> Occurring state
    -> List (Occurring state)
    -> List (Line state)
    -> motion
    -> motion
visitAll transitionOngoing toAnchor interp details prev states future state =
    -- let
    --     _ =
    --         log "------NEW - ALL" ""
    -- in
    case states of
        [] ->
            case future of
                [] ->
                    state

                (Line futureStart futureEvent futureRemain) :: restOfFuture ->
                    -- lerp to `next`
                    -- then start the next line
                    let
                        lerped =
                            state
                                |> interp.lerp
                                    (endTime prev)
                                    (toAnchor (getEvent prev))
                                    (toAnchor (getEvent futureEvent))
                                    (startTime futureEvent)
                                    (startTime futureEvent)
                                    (case futureRemain of
                                        [] ->
                                            Nothing

                                        next :: _ ->
                                            Just (lookAhead toAnchor next)
                                    )
                                |> interp.visit toAnchor
                                    futureEvent
                                    (endTime futureEvent)
                                    (case futureRemain of
                                        [] ->
                                            Nothing

                                        next :: _ ->
                                            Just (lookAhead toAnchor next)
                                    )
                    in
                    visitAll False
                        toAnchor
                        interp
                        details
                        futureEvent
                        futureRemain
                        restOfFuture
                        lerped

        top :: remain ->
            case future of
                [] ->
                    -- visit top and continue
                    let
                        visited =
                            interp.visit toAnchor
                                top
                                (endTime top)
                                (case remain of
                                    [] ->
                                        Nothing

                                    next :: _ ->
                                        Just (lookAhead toAnchor next)
                                )
                                state
                    in
                    visitAll False
                        toAnchor
                        interp
                        details
                        top
                        remain
                        future
                        visited

                (Line futureStart futureEvent futureRemain) :: restOfFuture ->
                    if Time.thisBeforeThat futureStart (endTime top) then
                        -- enroute to `top`, we are interrupted
                        -- so we lerp to top (stopping at the interruption point)
                        -- then continue on to the next line
                        let
                            lerped =
                                state
                                    |> interp.lerp
                                        (endTime prev)
                                        (toAnchor (getEvent prev))
                                        (toAnchor (getEvent top))
                                        (startTime top)
                                        futureStart
                                        (case remain of
                                            [] ->
                                                Nothing

                                            next :: _ ->
                                                Just (lookAhead toAnchor next)
                                        )
                                    |> interp.lerp
                                        futureStart
                                        (toAnchor (getEvent prev))
                                        (toAnchor (getEvent futureEvent))
                                        (startTime futureEvent)
                                        (startTime futureEvent)
                                        (case futureRemain of
                                            [] ->
                                                Nothing

                                            next :: _ ->
                                                Just (lookAhead toAnchor next)
                                        )
                        in
                        visitAll False
                            toAnchor
                            interp
                            details
                            prev
                            (futureEvent :: futureRemain)
                            restOfFuture
                            lerped

                    else
                        -- visit top and continue
                        let
                            visited =
                                interp.visit toAnchor
                                    top
                                    (endTime top)
                                    (case remain of
                                        [] ->
                                            Nothing

                                        next :: _ ->
                                            Just (lookAhead toAnchor next)
                                    )
                                    state
                        in
                        visitAll False
                            toAnchor
                            interp
                            details
                            top
                            remain
                            future
                            visited


log x y =
    -- Debug.log x y
    y


{-|

    At event A -> visit A
    After event A, no future -> visit A
    After event A, future event B ->
        visit A
        lerp @transition time, A -> B

Tricky part is when the transition "crosses lines"
After event A, future event B, new Line C starts ->

    visit A
    lerp @ transition1 A -> B
    lerp @ transition2 (A -> B) -> C

We also know that

1.  we are not before, at, or dwelling at prev.

This needs to be guaranteed by the caller of this function.

-}
throughLines :
    Bool
    -> Time.Absolute
    -> (state -> anchor)
    -> Interp state anchor motion
    -> TimelineDetails state
    -> Occurring state
    -> List (Occurring state)
    -> List (Line state)
    -> motion
    -> motion
throughLines transitionOngoing now toAnchor interp details prev states future state =
    case log "NEW STATES" states of
        [] ->
            case interruptedByFuture now future of
                NoInterruption ->
                    state

                AdvanceTo (Line interruptionTime futureEvent futureRemain) ->
                    throughLines False
                        now
                        toAnchor
                        interp
                        details
                        prev
                        (futureEvent :: futureRemain)
                        (List.drop 1 future)
                        state

                TransitionTo (Line interruptionTime futureEvent futureRemain) ->
                    let
                        visited =
                            if transitionOngoing then
                                state

                            else
                                interp.visit toAnchor
                                    prev
                                    interruptionTime
                                    Nothing
                                    state

                        lerped =
                            interp.lerp
                                interruptionTime
                                (toAnchor (getEvent prev))
                                (toAnchor (getEvent futureEvent))
                                (startTime futureEvent)
                                now
                                (case futureRemain of
                                    [] ->
                                        Nothing

                                    upcomingEvent :: _ ->
                                        Just (lookAhead toAnchor upcomingEvent)
                                )
                                visited

                        upcomingFuture =
                            List.drop 1 future

                        continuing =
                            case upcomingFuture of
                                [] ->
                                    False

                                (Line upcomingTransitionTime _ _) :: _ ->
                                    Time.thisAfterOrEqualThat now upcomingTransitionTime
                    in
                    if continuing then
                        throughLines True
                            now
                            toAnchor
                            interp
                            details
                            prev
                            (futureEvent :: futureRemain)
                            upcomingFuture
                            lerped

                    else
                        lerped

        start :: [] ->
            case interruptedByFuture now future of
                NoInterruption ->
                    -- visit start
                    -- nothing from the future has happened yet
                    state
                        -- visit start
                        -- we need to do this to enable the "previous" editor
                        -- which grabs the exactly previous state
                        |> interp.visit toAnchor
                            start
                            now
                            Nothing

                AdvanceTo (Line interruptionTime futureEvent futureRemain) ->
                    throughLines False
                        now
                        toAnchor
                        interp
                        details
                        start
                        (futureEvent :: futureRemain)
                        (List.drop 1 future)
                        state

                TransitionTo (Line interruptionTime futureEvent futureRemain) ->
                    if before now futureEvent then
                        -- We are actively transitioning from start to futureEvent!
                        -- visit start if transition is not ongoing
                        -- lerp to future
                        -- set transitionOngoing
                        -- -> continue
                        let
                            visited =
                                if transitionOngoing || before now start then
                                    state

                                else
                                    interp.visit toAnchor
                                        start
                                        interruptionTime
                                        Nothing
                                        state

                            actualPrevious =
                                if transitionOngoing || before now start then
                                    prev

                                else
                                    start

                            lerped =
                                interp.lerp
                                    interruptionTime
                                    (toAnchor (getEvent actualPrevious))
                                    (toAnchor (getEvent futureEvent))
                                    (startTime futureEvent)
                                    now
                                    (case futureRemain of
                                        [] ->
                                            Nothing

                                        upcomingEvent :: _ ->
                                            Just (lookAhead toAnchor upcomingEvent)
                                    )
                                    visited

                            upcomingFuture =
                                List.drop 1 future

                            continuing =
                                case upcomingFuture of
                                    [] ->
                                        False

                                    (Line upcomingTransitionTime _ _) :: _ ->
                                        Time.thisAfterOrEqualThat now upcomingTransitionTime
                        in
                        if continuing then
                            throughLines True
                                now
                                toAnchor
                                interp
                                details
                                actualPrevious
                                (futureEvent :: futureRemain)
                                upcomingFuture
                                lerped

                        else
                            lerped

                    else
                        -- We have already transitioned to the new line
                        throughLines False
                            now
                            toAnchor
                            interp
                            details
                            start
                            (futureEvent :: futureRemain)
                            (List.drop 1 future)
                            state

        start :: next :: remain ->
            case interruptedByFuture now future of
                NoInterruption ->
                    -- visit start
                    -- nothing from the future has happened yet
                    if during now start then
                        interp.visit toAnchor
                            start
                            now
                            (Just (lookAhead toAnchor next))
                            state

                    else if during now next || (after now next && List.isEmpty remain) then
                        state
                            -- visit start
                            -- we need to do this to enable the "previous" editor
                            -- which grabs the exactly previous state
                            |> interp.visit toAnchor
                                start
                                (endTime start)
                                (Just (lookAhead toAnchor next))
                            -- visit next
                            |> interp.visit toAnchor
                                next
                                now
                                (case remain of
                                    [] ->
                                        Nothing

                                    upcomingEvent :: _ ->
                                        Just (lookAhead toAnchor upcomingEvent)
                                )

                    else if transitioning now start next then
                        let
                            visited =
                                if transitionOngoing then
                                    state

                                else
                                    interp.visit toAnchor
                                        start
                                        now
                                        (Just (lookAhead toAnchor next))
                                        state
                        in
                        interp.lerp
                            (endTime start)
                            (toAnchor (getEvent start))
                            (toAnchor (getEvent next))
                            (startTime next)
                            now
                            (case remain of
                                [] ->
                                    Nothing

                                upcomingEvent :: _ ->
                                    Just (lookAhead toAnchor upcomingEvent)
                            )
                            visited

                    else
                        throughLines False
                            now
                            toAnchor
                            interp
                            details
                            start
                            (next :: remain)
                            future
                            state

                AdvanceTo (Line interruptionTime futureEvent futureRemain) ->
                    throughLines False
                        now
                        toAnchor
                        interp
                        details
                        start
                        (futureEvent :: futureRemain)
                        (List.drop 1 future)
                        state

                TransitionTo (Line interruptionTime futureEvent futureRemain) ->
                    let
                        upcomingFuture =
                            List.drop 1 future

                        continuationTime =
                            case upcomingFuture of
                                [] ->
                                    now

                                (Line nextTime _ _) :: _ ->
                                    if Time.thisBeforeThat nextTime now then
                                        nextTime

                                    else
                                        now

                        continuing =
                            case upcomingFuture of
                                [] ->
                                    False

                                (Line nextTime _ _) :: _ ->
                                    if Time.thisBeforeThat nextTime now then
                                        True

                                    else
                                        False
                    in
                    if during now start then
                        let
                            visited =
                                if transitionOngoing then
                                    state

                                else
                                    interp.visit toAnchor
                                        start
                                        now
                                        (Just (lookAhead toAnchor next))
                                        state

                            actualPrevious =
                                if transitionOngoing then
                                    prev

                                else
                                    start

                            lerped =
                                interp.lerp
                                    interruptionTime
                                    (toAnchor (getEvent actualPrevious))
                                    (toAnchor (getEvent futureEvent))
                                    (startTime futureEvent)
                                    now
                                    (case futureRemain of
                                        [] ->
                                            Nothing

                                        upcomingEvent :: _ ->
                                            Just (lookAhead toAnchor upcomingEvent)
                                    )
                                    visited
                        in
                        if continuing then
                            throughLines True
                                now
                                toAnchor
                                interp
                                details
                                actualPrevious
                                (futureEvent :: futureRemain)
                                (List.drop 1 future)
                                lerped

                        else
                            lerped

                    else if transitioning interruptionTime start next then
                        let
                            visited =
                                if transitionOngoing then
                                    state

                                else
                                    interp.visit toAnchor
                                        start
                                        (endTime start)
                                        (Just (lookAhead toAnchor next))
                                        state

                            actualPrevious =
                                if transitionOngoing then
                                    prev

                                else
                                    start

                            lerped =
                                visited
                                    |> interp.lerp
                                        (endTime actualPrevious)
                                        (toAnchor (getEvent actualPrevious))
                                        (toAnchor (getEvent next))
                                        (startTime next)
                                        interruptionTime
                                        (case remain of
                                            [] ->
                                                Nothing

                                            upcomingEvent :: _ ->
                                                Just (lookAhead toAnchor upcomingEvent)
                                        )
                                    |> interp.lerp
                                        interruptionTime
                                        (toAnchor (getEvent actualPrevious))
                                        (toAnchor (getEvent futureEvent))
                                        (startTime futureEvent)
                                        continuationTime
                                        (case futureRemain of
                                            [] ->
                                                Nothing

                                            upcomingEvent :: _ ->
                                                Just (lookAhead toAnchor upcomingEvent)
                                        )
                        in
                        if continuing then
                            throughLines True
                                now
                                toAnchor
                                interp
                                details
                                actualPrevious
                                (futureEvent :: futureRemain)
                                future
                                lerped

                        else
                            lerped

                    else
                        -- OTHERWISE!
                        -- we need to fastforward on this line to find the event that happens right before the transition
                        throughLines False
                            now
                            toAnchor
                            interp
                            details
                            start
                            (next :: remain)
                            future
                            state


type Interruption event
    = NoInterruption
      -- we're not transitioning to this line, we're already there in some capacity!
    | AdvanceTo (Line event)
    | TransitionTo (Line event)


interruptedByFuture : Time.Absolute -> List (Line event) -> Interruption event
interruptedByFuture now future =
    log "INTERR" <|
        case future of
            [] ->
                NoInterruption

            ((Line futureStart first _) as line) :: remain ->
                if Time.thisAfterOrEqualThat now futureStart then
                    if before now first then
                        TransitionTo line

                    else
                        case remain of
                            [] ->
                                AdvanceTo line

                            ((Line nextStart second _) as upcomingLine) :: _ ->
                                -- if the next transition happens BEFORE the start of the `first` event,
                                -- then we actually need to start transitioning
                                if
                                    Time.thisAfterOrEqualThat now nextStart
                                        && before nextStart first
                                then
                                    TransitionTo line

                                else
                                    AdvanceTo line

                else
                    NoInterruption


interruptedByFutureOld : Time.Absolute -> List (Line event) -> Maybe (Line event)
interruptedByFutureOld now future =
    case future of
        [] ->
            Nothing

        ((Line futureStart _ _) as line) :: _ ->
            if Time.thisAfterOrEqualThat now futureStart then
                Just line

            else
                Nothing


before : Time.Absolute -> Occurring event -> Bool
before now (Occurring _ start end) =
    Time.thisBeforeThat now start


after : Time.Absolute -> Occurring event -> Bool
after now (Occurring _ start end) =
    Time.thisAfterThat now end


during : Time.Absolute -> Occurring event -> Bool
during now (Occurring _ start end) =
    Time.thisAfterOrEqualThat now start
        && Time.thisBeforeOrEqualThat now end


transitioning : Time.Absolute -> Occurring event -> Occurring event -> Bool
transitioning now (Occurring _ _ end) (Occurring _ start _) =
    Time.thisAfterOrEqualThat now end
        && Time.thisBeforeOrEqualThat now start


createLookAhead fn lookup currentEvent upcomingEvents =
    case upcomingEvents of
        [] ->
            Nothing

        unadjustedUpcoming :: remain ->
            let
                upcomingOccurring =
                    adjustTimeWithPrevious lookup fn.adjustor currentEvent unadjustedUpcoming remain
            in
            Just
                { anchor = lookup (getEvent upcomingOccurring)
                , time = startTime upcomingOccurring

                -- (startTimeAdj lookup fn.adjustor currentEvent upcomingOccurring)
                , resting =
                    hasDwell upcomingOccurring
                }


{-| Some notes about this function:

  - `visit` must always be called with adjusted values.
  - `lerp` must always be called with adjusted values.
  - `createLookAhead` must always be caled with UNadjsuted values.
    Basically any function that's in `Interp` is expected values to already be adjusted.

-}
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
overLines fn lookup details maybePreviousEvent (Line lineStart unadjustedStartEvent lineRemain) futureLines state =
    -- futureStart starts a new line.
    -- if an interruption occurs, we want to interpolate to the point of the interruption
    -- then transition over to the new line.
    let
        -- _ =
        --     log "------" "OLD"
        -- _ =
        --     log "    ->" now
        transition prev newState =
            -- After we interpolate, we check here if we are actually done
            -- or if we were just interrupted and need to keep going
            case futureLines of
                [] ->
                    newState

                ((Line futureStart futureStartEv futureRemain) as future) :: restOfFuture ->
                    if Time.thisBeforeOrEqualThat futureStart details.now then
                        overLines fn lookup details (Just prev) future restOfFuture newState

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

        lineStartEv =
            case maybePreviousEvent of
                Nothing ->
                    adjustTime lookup fn.adjustor unadjustedStartEvent lineRemain

                Just prev ->
                    adjustTimeWithPrevious lookup fn.adjustor prev unadjustedStartEvent lineRemain
    in
    if Time.thisBeforeThat now (startTime lineStartEv) then
        -- lerp from state to lineStartEv
        -- now is before the first event start time.
        fn.lerp
            lineStart
            (case maybePreviousEvent of
                Nothing ->
                    lookup details.initial

                Just p ->
                    lookup (getEvent p)
            )
            (lookup (getEvent lineStartEv))
            (startTime lineStartEv)
            now
            (createLookAhead fn lookup unadjustedStartEvent lineRemain)
            state
            |> transition lineStartEv

    else if Time.thisBeforeThat now (endTime lineStartEv) then
        -- dwell at linestartEv
        -- we've checked that it's not after or before,
        -- so it has to be between the start and end
        fn.visit lookup
            lineStartEv
            now
            (createLookAhead fn lookup unadjustedStartEvent lineRemain)
            state
            |> transition lineStartEv

    else
        -- after linestartEv
        case lineRemain of
            [] ->
                -- dwell at lineStartEv, there's nothing to transition to
                fn.visit lookup
                    lineStartEv
                    now
                    Nothing
                    state
                    |> transition lineStartEv

            unadjustedNext :: lineRemain2 ->
                let
                    next =
                        adjustTimeWithPrevious lookup fn.adjustor unadjustedStartEvent unadjustedNext lineRemain2
                in
                if Time.thisBeforeThat now (startTime next) then
                    -- Before next.startTime
                    --     -> lerp start to next
                    fn.lerp
                        (endTime lineStartEv)
                        (lookup (getEvent lineStartEv))
                        (lookup (getEvent next))
                        (startTime next)
                        now
                        (createLookAhead fn lookup unadjustedNext lineRemain2)
                        (fn.visit lookup
                            lineStartEv
                            now
                            (createLookAhead fn lookup unadjustedStartEvent lineRemain)
                            state
                        )
                        |> transition lineStartEv

                else if Time.thisBeforeThat now (endTime next) then
                    -- After next.startTime
                    --- Before next.endTime
                    --      -> we're dwelling at `next`
                    fn.visit lookup
                        next
                        now
                        (createLookAhead fn lookup unadjustedNext lineRemain2)
                        state
                        |> transition lineStartEv

                else
                    -- After lineStart.endTime
                    -- After next.startTime
                    -- After next.endTime
                    case lineRemain2 of
                        [] ->
                            -- Nothing to continue on to,
                            fn.visit lookup
                                next
                                now
                                Nothing
                                state
                                |> transition next

                        unadjustedNext2 :: lineRemain3 ->
                            let
                                next2 =
                                    adjustTimeWithPrevious
                                        lookup
                                        fn.adjustor
                                        unadjustedNext
                                        unadjustedNext2
                                        lineRemain3
                            in
                            if Time.thisBeforeThat now (startTime next2) then
                                let
                                    afterState =
                                        fn.visit lookup
                                            next
                                            now
                                            (createLookAhead fn lookup unadjustedNext lineRemain2)
                                            state
                                in
                                fn.lerp
                                    -- next end time?
                                    (endTime next)
                                    (lookup (getEvent next))
                                    (lookup (getEvent next2))
                                    (startTime next2)
                                    now
                                    (createLookAhead fn lookup unadjustedNext2 lineRemain3)
                                    afterState
                                    |> transition next

                            else if Time.thisBeforeThat now (endTime next2) then
                                -- we're dwelling at `next2`
                                fn.visit lookup
                                    next2
                                    now
                                    (createLookAhead fn lookup unadjustedNext2 lineRemain3)
                                    state
                                    |> transition next2

                            else
                                let
                                    afterState =
                                        fn.visit lookup
                                            next2
                                            now
                                            (createLookAhead fn lookup unadjustedNext2 lineRemain3)
                                            state
                                in
                                -- Recompose our line by removing previous events
                                -- and continue forward!
                                let
                                    _ =
                                        log "ff" "continue"
                                in
                                overLines
                                    fn
                                    lookup
                                    details
                                    (Just next)
                                    (Line (endTime next) unadjustedNext2 lineRemain3)
                                    futureLines
                                    afterState


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

                                    lastConcreteState =
                                        overLines
                                            fn
                                            lookup
                                            -- maybe we break this out to a separate param to avoid updating?
                                            { timelineDetails | now = dwellStartTime }
                                            Nothing
                                            firstLine
                                            remainingLines
                                            start

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
                                                fn.visit lookup
                                                    lastEvent
                                                    currentTime
                                                    Nothing
                                                    lastConcreteState
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


type Status
    = Dwelling Time.Duration
    | Transitioning Float


linearDefault : Personality
linearDefault =
    { departLate = 0
    , departSlowly = 0
    , wobbliness = 0
    , impulse = 0
    , arriveEarly = 0
    , arriveSlowly = 0
    }



{- BOOKKEEPING -}


arrived : Timeline event -> event
arrived ((Timeline details) as timeline) =
    foldp
        identity
        { start =
            \_ ->
                details.initial
        , dwellPeriod = \_ -> Nothing
        , adjustor =
            \_ ->
                linearDefault
        , visit =
            \lookup target targetTime maybeLookAhead state ->
                getEvent target
        , lerp =
            \prevEndTime prev target targetTime now maybeLookAhead state ->
                prev
        }
        timeline


current : Timeline event -> event
current ((Timeline details) as timeline) =
    foldp
        identity
        { start =
            \_ ->
                details.initial
        , dwellPeriod = \_ -> Nothing
        , adjustor =
            \_ ->
                linearDefault
        , visit =
            \lookup target now maybeLookAhead state ->
                getEvent target
        , lerp =
            \_ maybePrevious target _ _ _ state ->
                target
        }
        timeline


previous : Timeline event -> event
previous ((Timeline details) as timeline) =
    foldp
        identity
        { start =
            \start ->
                { prev = start
                , current = start
                , transitioning = False
                }
        , dwellPeriod = \_ -> Nothing
        , adjustor =
            \_ ->
                linearDefault
        , visit =
            \lookup target now maybeLookAhead state ->
                { current = lookup (getEvent target)
                , prev = state.current
                , transitioning = False
                }
        , lerp =
            \lookup prev target _ _ _ state ->
                -- if we've begun trasitioning to another state,
                -- then the previous state is the last visited.
                if state.transitioning then
                    -- we've been interrupted a second time,
                    -- this doesnt change the previous state
                    state

                else
                    -- we're transitioning, the previous state is the last visited
                    { current = state.current
                    , prev = state.current
                    , transitioning = True
                    }
        }
        timeline
        |> .prev


arrivedAt : (event -> Bool) -> Time.Posix -> Timeline event -> Bool
arrivedAt matches newTime (Timeline details) =
    foldp
        identity
        { start =
            \_ ->
                False
        , dwellPeriod = \_ -> Nothing
        , adjustor =
            \_ ->
                linearDefault
        , visit =
            \lookup target _ maybeLookAhead state ->
                matches (getEvent target)
                    && Time.thisBeforeThat details.now (startTime target)
        , lerp =
            \_ _ target targetTime now _ state ->
                state
                    || (matches target
                            && Time.thisBeforeOrEqualThat details.now targetTime
                            && Time.thisAfterOrEqualThat (Time.absolute newTime) targetTime
                       )
        }
        (Timeline details)


onMaybe fn maybe =
    case maybe of
        Nothing ->
            False

        Just thing ->
            fn thing


matchesEvent matches (Event _ event _) =
    matches event


anyScheduled : (event -> Bool) -> Schedule event -> Bool
anyScheduled matches (Schedule dur startEvent remainingEvents) =
    if matchesEvent matches startEvent then
        True

    else
        List.any (matchesEvent matches) remainingEvents


{-| -}
upcoming : (event -> Bool) -> Timeline event -> Bool
upcoming matches (Timeline details) =
    -- we check both the queued and interruption caches
    -- This function is sometimes used to prevent queueing an action multiple times
    -- However if multiple msgs get fired in one frame, then there's still a subtle possibility that something will get double queued.
    if onMaybe (anyScheduled matches) details.queued then
        True

    else if List.any (anyScheduled matches) details.interruption then
        True

    else
        foldp
            identity
            { start =
                \_ ->
                    False
            , dwellPeriod = \_ -> Nothing
            , adjustor =
                \_ ->
                    linearDefault
            , visit =
                \lookup target _ maybeLookAhead state ->
                    matches (getEvent target)
                        && Time.thisBeforeThat details.now (startTime target)
            , lerp =
                \_ _ target targetTime _ _ state ->
                    state
                        || (matches target
                                && Time.thisBeforeThat details.now targetTime
                           )
            }
            (Timeline { details | now = Time.millis (1 / 0) })


status : Timeline event -> Status
status ((Timeline details) as timeline) =
    foldp
        identity
        { start =
            \_ ->
                Transitioning 0
        , dwellPeriod = \_ -> Nothing
        , adjustor =
            \_ ->
                linearDefault
        , visit =
            \lookup (Occurring event start eventEnd) now maybeLookAhead state ->
                let
                    dwellTime =
                        case maybeLookAhead of
                            Nothing ->
                                Time.duration start now

                            _ ->
                                Time.duration start (Time.earliest now eventEnd)
                in
                if Time.zeroDuration dwellTime then
                    Transitioning 1

                else
                    Dwelling dwellTime
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
                if Time.thisAfterOrEqualThat now end then
                    Transitioning 1

                else
                    Transitioning (Time.progress start end now)
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
