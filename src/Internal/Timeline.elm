module Internal.Timeline exposing
    ( Timeline(..), TimelineDetails, Occurring(..), getEvents
    , Schedule(..), Event(..)
    , needsUpdate, update, updateWith
    , startTime, endTime, getEvent, extendEventDwell, hasDwell, isResting
    , addToDwell
    , sendPing
    , current, arrivedAt, arrived, previous, upcoming
    , Line(..), Timetable(..)
    , foldpAll, captureTimeline
    , ActualDuration(..), Animator(..), Description(..), Frame(..), Frames(..), FramesSummary, Interp, LookAhead, Period(..), Previous(..), Resting(..), Summary, SummaryEvent(..), Transition, atTime, combineRunning, dwellingTime, gc, getCurrentTime, hasChanged, justInitialized, linesAreActive, periodDuration, progress
    )

{-|

@docs Timeline, TimelineDetails, Occurring, getEvents

@docs Schedule, Event

@docs needsUpdate, update, updateWith

@docs startTime, endTime, getEvent, extendEventDwell, hasDwell, isResting

@docs addToDwell

@docs sendPing

@docs current, arrivedAt, arrived, previous, upcoming

@docs Line, Timetable

@docs foldpAll, captureTimeline

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
    , visit : Visit state anchor motion
    , transition : Lerp anchor motion
    }


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


type Previous event
    = Previous (Occurring event)
    | PreviouslyInterrupted Time.Absolute


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


isResting : Occurring event -> Bool
isResting (Occurring _ (Quantity.Quantity start) (Quantity.Quantity end)) =
    (end - start) == 0


startTime : Occurring event -> Time.Absolute
startTime (Occurring _ time _) =
    time


endTime : Occurring event -> Time.Absolute
endTime (Occurring _ _ end) =
    end


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
        | running =
            running
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
                    enqueue timeline timeline.now queued
                , queued = Nothing
            }


{-|

    *NOTE* - this only looks at the most immediately upcoming event and does an equality check.
    There may be other cases we want to cover here, though this is the most common one by far.
    However, others could be captured by manually checking if events are `Timeline.upcoming`

-}
scheduleMatchesExisting : Schedule event -> Line event -> Bool
scheduleMatchesExisting (Schedule scheduleDelay event schedulUpcoming) (Line lineStart lineStartEvent lineUpcoming) =
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
scheduledEventEqual (Event dur schedEvent maybeDwell) (Occurring occurEvent occurStart occurEnd) =
    schedEvent == occurEvent


interruptionHappensLaterDebug : Time.Absolute -> List (Line event) -> Bool
interruptionHappensLaterDebug startInterruption remaining =
    case remaining of
        [] ->
            False

        top :: _ ->
            Time.thisAfterOrEqualThat startInterruption (lineStartTime top)


interruptLineDebug : Time.Absolute -> Schedule event -> Line event -> List (Line event) -> Maybe (List (Line event))
interruptLineDebug startInterruption scheduled line future =
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



{- INTERRUPTION DEBUG2 -}


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


rescale : Time.Absolute -> Float -> List (Line event) -> List (Line event)
rescale now scale lines =
    if scale == 1 then
        lines

    else
        List.map (rescaleLine now scale) lines


rescaleLine : Time.Absolute -> Float -> Line event -> Line event
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

                beginning =
                    case timetable of
                        [] ->
                            Occurring timelineDetails.initial timelineDetails.now timelineDetails.now

                        (Line lineStart firstEvent remain) :: _ ->
                            Occurring timelineDetails.initial lineStart lineStart
            in
            visitAll2
                lookup
                transitionTo
                timelineDetails
                beginning
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



{- BOOKKEEPING -}


type Status
    = Dwelling Time.Duration
    | Transitioning Float


status : Timeline event -> Status
status ((Timeline details) as timeline) =
    foldpAll identity
        (\_ -> Dwelling Time.zeroDuration)
        (\lookup prev target now start end future state ->
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
        (\lookup prev target now start end future state ->
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
        (\lookup prev target now start end future state ->
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
        (\lookup prev target now start end future state ->
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
        (\lookup prev target now start end future state ->
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
anyScheduled matches (Schedule dur startEvent remainingEvents) =
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
            (\lookup prev target now start end future state ->
                state
                    || (matches (getEvent target)
                            && Time.thisBeforeThat now end
                       )
            )
            tl



{- ANIMATOR -}
{- The animator checks to see if any timelines are running and also has the ability to update the animation state.


   Different animators can do different things


      - Normal -> always on
      - Inline -> on when moving (require anotation of dwelling events)
      - CSS    -> single update when timeline is updated


-}


{-| -}
type Animator model
    = Animator (model -> Running) (Time.Posix -> model -> model)


type alias Running =
    { running : Bool
    , ping : Maybe { delay : Float, target : Time.Posix }
    }


combineRunning : Running -> Running -> Running
combineRunning one two =
    { running = one.running || two.running
    , ping =
        case two.ping of
            Nothing ->
                one.ping

            Just twoPing ->
                case one.ping of
                    Nothing ->
                        Just twoPing

                    Just onePing ->
                        if onePing.delay < twoPing.delay then
                            Just onePing

                        else
                            Just twoPing
    }


sendPing : Timeline event -> Maybe { delay : Float, target : Time.Posix }
sendPing ((Timeline details) as timeline) =
    Maybe.andThen (encodeStamp details.now) (findNextTransitionTime timeline)


findNextTransitionTime : Timeline event -> Maybe Time.Absolute
findNextTransitionTime ((Timeline details) as timeline) =
    foldpAll identity
        (\_ -> Nothing)
        (\lookup prev target now start end future state ->
            if Time.thisBeforeThat now end && (startTime target == end) then
                Just (startTime target)

            else
                state
        )
        timeline


{-| This is to account for a bug in `Time.every` where it uses the provided delay time as a key to keep track of a time.

To get around this, we encode the current time as a really small part of the given time.

-}
encodeStamp : Time.Absolute -> Time.Absolute -> Maybe { delay : Float, target : Time.Posix }
encodeStamp now target =
    let
        millis =
            Time.inMilliseconds target - nowInMillis

        nowInMillis =
            Time.inMilliseconds now

        nowTail =
            nowInMillis / 1000000

        pingDelay =
            millis + (1 / nowTail)
    in
    if pingDelay <= 0 then
        Nothing

    else
        Just
            { delay = pingDelay
            , target =
                Time.millisToPosix (round (Time.inMilliseconds target + 1))
            }


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
