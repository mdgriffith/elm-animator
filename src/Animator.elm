module Animator exposing
    ( Timeline, init
    , current
    , Animator, animator, with, toSubscription, update
    , to
    , Duration, millis, seconds, minutes
    , Step, wait, event, interrupt, queue
    , color
    , linear, float
    , Movement, move, xy, xyz
    , details
    , at
    , Proportion
    , leaveSmoothly, leaveLate
    , arriveSmoothly, arriveEarly
    , withWobble
    , wave, wrap, zigzag
    , loop, repeat, once
    , pause, shift
    , step
    , Frames, frame, hold, walk, framesWith
    , Resting, FramesPerSecond, fps, cycle, cycleN
    )

{-|

@docs Timeline, init, initWith

@docs current


# Animating

@docs Animator, animator, with, toSubscription, update


# Adding events to a timeline

@docs to

@docs Duration, millis, seconds, minutes

@docs Step, wait, event, interrupt, queue


# Animating

@docs color

@docs linear, float

@docs Movement, move, xy, xyz

@docs details


# Adjusting transition

@docs at

@docs Proportion

@docs leaveSmoothly, leaveLate

@docs arriveSmoothly, arriveEarly

@docs withWobble


# Resting at a state

@docs wave, wrap, zigzag

@docs loop, repeat, once

@docs pause, shift


# Sprites

@docs step

@docs Frames, frame, hold, walk, framesWith

@docs Resting, FramesPerSecond, fps, cycle, cycleN

-}

import Browser.Events
import Color exposing (Color)
import Duration
import Internal.Interpolate as Interpolate
import Internal.Time as Time
import Internal.Timeline as Timeline
import Quantity
import Time


{-| -}
type alias Timeline state =
    Timeline.Timeline state


{-| -}
init : state -> Timeline state
init first =
    Timeline.Timeline
        { initial = first
        , now = Time.absolute (Time.millisToPosix 0)
        , events =
            Timeline.Timetable []
        , queued = Nothing
        , interruption = []
        , running = True
        }


{-| -}
initWith : Time.Posix -> state -> Timeline state
initWith now first =
    Timeline.Timeline
        { initial = first
        , now = Time.absolute (Time.millisToPosix 0)
        , events =
            Timeline.Timetable []
        , queued = Nothing
        , interruption = []
        , running = True
        }
        |> Timeline.update now


{-| -}
current : Timeline state -> state
current =
    Timeline.current


{-| -}
type alias Duration =
    Time.Duration


{-| -}
millis : Float -> Duration
millis =
    Duration.milliseconds


{-| -}
seconds : Float -> Duration
seconds =
    Duration.seconds


{-| -}
minutes : Float -> Duration
minutes =
    Duration.minutes


{-| -}
type Step state
    = Wait Duration
    | TransitionTo Duration state


{-| -}
event : Duration -> state -> Step state
event =
    TransitionTo


{-| -}
wait : Duration -> Step state
wait =
    Wait


{-| -}
queue : List (Step state) -> Timeline state -> Timeline state
queue steps (Timeline.Timeline tl) =
    Timeline.Timeline
        { tl
            | running = True
            , queued =
                case tl.queued of
                    Nothing ->
                        case initializeSchedule (millis 0) steps of
                            Nothing ->
                                tl.queued

                            Just ( schedule, otherSteps ) ->
                                Just (List.foldl stepsToEvents schedule otherSteps)

                    Just queued ->
                        Just (List.foldl stepsToEvents queued steps)
        }


{-| -}
to : Duration -> state -> Timeline state -> Timeline state
to dur ev timeline =
    interrupt [ event dur ev ] timeline


{-| -}
interrupt : List (Step state) -> Timeline state -> Timeline state
interrupt steps (Timeline.Timeline tl) =
    Timeline.Timeline
        { tl
            | running = True
            , interruption =
                case initializeSchedule (millis 0) steps of
                    Nothing ->
                        tl.interruption

                    Just ( schedule, otherSteps ) ->
                        List.foldl stepsToEvents schedule otherSteps :: tl.interruption
        }


initializeSchedule : Time.Duration -> List (Step state) -> Maybe ( Schedule state, List (Step state) )
initializeSchedule waiting steps =
    case steps of
        [] ->
            Nothing

        (Wait additionalWait) :: moreSteps ->
            initializeSchedule
                (Quantity.plus waiting additionalWait)
                moreSteps

        (TransitionTo dur checkpoint) :: moreSteps ->
            Just ( Timeline.Schedule waiting (Timeline.Event dur checkpoint Nothing) [], moreSteps )


stepsToEvents : Step state -> Timeline.Schedule state -> Timeline.Schedule state
stepsToEvents currentStep (Timeline.Schedule delay startEvent events) =
    case events of
        [] ->
            case currentStep of
                Wait waiting ->
                    Timeline.Schedule
                        delay
                        (Timeline.extendEventDwell waiting startEvent)
                        events

                TransitionTo dur checkpoint ->
                    Timeline.Schedule
                        delay
                        startEvent
                        [ Timeline.Event dur checkpoint Nothing ]

        (Timeline.Event durationTo recentEvent maybeDwell) :: remaining ->
            case currentStep of
                Wait dur ->
                    Timeline.Schedule
                        delay
                        startEvent
                        (Timeline.Event durationTo recentEvent (Timeline.addToDwell dur maybeDwell) :: remaining)

                TransitionTo dur checkpoint ->
                    if checkpoint == recentEvent then
                        Timeline.Schedule
                            delay
                            startEvent
                            (Timeline.Event durationTo recentEvent (Timeline.addToDwell dur maybeDwell) :: remaining)

                    else
                        Timeline.Schedule
                            delay
                            startEvent
                            (Timeline.Event dur checkpoint Nothing :: events)


{-| -}
type alias Event state =
    Timeline.Event state


{-| -}
type alias Schedule state =
    Timeline.Schedule state



{- Interpolations -}


type alias Description state =
    Timeline.Description state


{-| -}
color : Timeline state -> (state -> Color) -> Color
color timeline lookup =
    Timeline.foldpSlim
        lookup
        Interpolate.coloring
        timeline


{-| Interpolate a float linearly between destinations.
-}
linear : Timeline state -> (state -> Float) -> Float
linear timeline lookup =
    Timeline.foldpSlim
        lookup
        Interpolate.linearly2
        timeline


{-| -}
float : Timeline state -> (state -> Float) -> Float
float timeline lookup =
    .position <|
        details timeline (\ev -> at (lookup ev))


{-| -}
move : Timeline state -> (state -> Movement) -> Float
move timeline lookup =
    .position <|
        details timeline lookup


{-| -}
xy : Timeline state -> (state -> { x : Movement, y : Movement }) -> { x : Float, y : Float }
xy timeline lookup =
    { x =
        Timeline.foldpSlim
            (lookup >> .x)
            Interpolate.moving
            timeline
            |> unwrapUnits
            |> .position
    , y =
        Timeline.foldpSlim
            (lookup >> .y)
            Interpolate.moving
            timeline
            |> unwrapUnits
            |> .position
    }


{-| -}
xyz : Timeline state -> (state -> { x : Movement, y : Movement, z : Movement }) -> { x : Float, y : Float, z : Float }
xyz timeline lookup =
    { x =
        Timeline.foldpSlim
            (lookup >> .x)
            Interpolate.moving
            timeline
            |> unwrapUnits
            |> .position
    , y =
        Timeline.foldpSlim
            (lookup >> .y)
            Interpolate.moving
            timeline
            |> unwrapUnits
            |> .position
    , z =
        Timeline.foldpSlim
            (lookup >> .z)
            Interpolate.moving
            timeline
            |> unwrapUnits
            |> .position
    }


{-| -}
details : Timeline state -> (state -> Movement) -> { position : Float, velocity : Float }
details timeline lookup =
    unwrapUnits
        (Timeline.foldpSlim
            lookup
            Interpolate.moving
            timeline
        )


unwrapUnits { position, velocity } =
    { position =
        case position of
            Quantity.Quantity val ->
                val
    , velocity =
        case velocity of
            Quantity.Quantity val ->
                val
    }


{-| -}
type alias Movement =
    Interpolate.Movement


{-| -}
at : Float -> Movement
at =
    Interpolate.Position Interpolate.defaultDeparture Interpolate.defaultArrival



{- PERSONALITY -}


{-| This is an alias for a `Float` between `0` and `1`.

Behind the scenes it will be clamped at those values.

-}
type alias Proportion =
    Float


{-| -}
withWobble : Proportion -> Movement -> Movement
withWobble p movement =
    case movement of
        Interpolate.Position dep arrival pos ->
            Interpolate.Position dep { arrival | wobbliness = clamp 0 1 p } pos

        Interpolate.Oscillate dep arrival dur fn ->
            Interpolate.Oscillate dep { arrival | wobbliness = clamp 0 1 p } dur fn



-- {-| -}
-- smooth : Proportion
-- smooth =
--     0.4
-- {-| -}
-- verySmooth : Proportion
-- verySmooth =
--     0.8


{-| -}
leaveLate : Proportion -> Movement -> Movement
leaveLate p movement =
    case movement of
        Interpolate.Position dep arrival pos ->
            Interpolate.Position { dep | late = clamp 0 1 p } arrival pos

        Interpolate.Oscillate dep arrival dur fn ->
            Interpolate.Oscillate { dep | late = clamp 0 1 p } arrival dur fn


{-| -}
arriveEarly : Proportion -> Movement -> Movement
arriveEarly p movement =
    case movement of
        Interpolate.Position dep arrival pos ->
            Interpolate.Position dep { arrival | early = clamp 0 1 p } pos

        Interpolate.Oscillate dep arrival dur fn ->
            Interpolate.Oscillate dep { arrival | early = clamp 0 1 p } dur fn


{-| -}
leaveSmoothly : Proportion -> Movement -> Movement
leaveSmoothly s movement =
    case movement of
        Interpolate.Position dep arrival pos ->
            Interpolate.Position { dep | slowly = clamp 0 1 s } arrival pos

        Interpolate.Oscillate dep arrival dur fn ->
            Interpolate.Oscillate { dep | slowly = clamp 0 1 s } arrival dur fn


{-| -}
arriveSmoothly : Proportion -> Movement -> Movement
arriveSmoothly s movement =
    case movement of
        Interpolate.Position dep arrival pos ->
            Interpolate.Position dep { arrival | slowly = clamp 0 1 s } pos

        Interpolate.Oscillate dep arrival dur fn ->
            Interpolate.Oscillate dep { arrival | slowly = clamp 0 1 s } dur fn


{-| -}
type Oscillator
    = Oscillator (List Pause) (Float -> Float)


{-| -}
type Pause
    = Pause Duration Float


within : Float -> Float -> Float -> Bool
within tolerance anchor val =
    let
        low =
            anchor - tolerance

        high =
            anchor + tolerance
    in
    val >= low && val <= high


pauseToBounds : Pause -> Duration -> Duration -> ( Float, Float )
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


{-| -}
once : Duration -> Oscillator -> Movement
once activeDuration osc =
    let
        ( fn, totalDuration ) =
            prepareOscillator activeDuration osc
    in
    Interpolate.Oscillate Interpolate.defaultDeparture
        Interpolate.defaultArrival
        (Timeline.Repeat 1 totalDuration)
        fn


{-| -}
loop : Duration -> Oscillator -> Movement
loop activeDuration osc =
    let
        ( fn, totalDuration ) =
            prepareOscillator activeDuration osc
    in
    Interpolate.Oscillate Interpolate.defaultDeparture
        Interpolate.defaultArrival
        (Timeline.Loop totalDuration)
        fn


{-| -}
repeat : Int -> Duration -> Oscillator -> Movement
repeat n activeDuration osc =
    let
        ( fn, totalDuration ) =
            prepareOscillator activeDuration osc
    in
    Interpolate.Oscillate Interpolate.defaultDeparture
        Interpolate.defaultArrival
        (Timeline.Repeat n totalDuration)
        fn


prepareOscillator : Duration -> Oscillator -> ( Float -> Float, Duration )
prepareOscillator activeDuration (Oscillator pauses osc) =
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


{-| Shift an oscillator over by a certain amount.

It's expecting a number between 0 and 1.

-}
shift : Proportion -> Oscillator -> Oscillator
shift x (Oscillator pauses osc) =
    Oscillator
        pauses
        (\u -> osc (wrapToUnit (u + x)))


wrapToUnit : Float -> Float
wrapToUnit x =
    x - toFloat (floor x)


{-| When the oscillator is at a certain point, pause.

This pause time will be added to the time you specify using `loop`, so that you can adjust the pause without disturbing the original duration of the oscillator.

-}
pause : Duration -> Proportion -> Oscillator -> Oscillator
pause forDuration val (Oscillator pauses osc) =
    Oscillator
        (Pause forDuration val :: pauses)
        osc


{-| Start at one number and move linearly to another, then wrap back to the first.
-}
wrap : Float -> Float -> Oscillator
wrap start end =
    let
        total =
            end - start
    in
    Oscillator []
        (\u ->
            start + (total * u)
        )


{-| This is basically a sine wave!
-}
wave : Float -> Float -> Oscillator
wave start end =
    let
        top =
            max start end

        bottom =
            min start end

        total =
            top - bottom
    in
    Oscillator []
        (\u ->
            let
                normalized =
                    (cos (turns (0.5 + u)) + 1) / 2
            in
            start + total * normalized
        )


{-| Start at one number, move linearly to another, and then linearly back.
-}
zigzag : Float -> Float -> Oscillator
zigzag start end =
    let
        total =
            end - start
    in
    Oscillator []
        (\u ->
            start + total * (1 - abs (2 * u - 1))
        )



{- SPRITES -}


{-| -}
type Frames item
    = Frame item
    | Hold Int item
    | Walk item (List (Frames item))
    | WithRest (Resting item) (Frames item)


{-| -}
type Resting item
    = Cycle Timeline.Period (List (Frames item))


{-| -}
frame : sprite -> Frames sprite
frame =
    Frame


{-| -}
hold : Int -> sprite -> Frames sprite
hold =
    Hold


{-| -}
walk : sprite -> List (Frames sprite) -> Frames sprite
walk =
    Walk


{-| -}
framesWith :
    { transition : Frames item
    , resting : Resting item
    }
    -> Frames item
framesWith cfg =
    WithRest
        cfg.resting
        cfg.transition


{-| -}
type FramesPerSecond
    = FramesPerSecond Float


{-| -}
fps : Float -> FramesPerSecond
fps =
    FramesPerSecond


{-| -}
cycle : FramesPerSecond -> List (Frames sprite) -> Resting sprite
cycle (FramesPerSecond framesPerSecond) frames =
    let
        duration =
            Duration.seconds (toFloat (List.length frames) / framesPerSecond)
    in
    Cycle (Timeline.Loop duration) frames


{-| -}
cycleN : Int -> FramesPerSecond -> List (Frames sprite) -> Resting sprite
cycleN n (FramesPerSecond framesPerSecond) frames =
    let
        duration =
            Duration.seconds (toFloat (List.length frames) / framesPerSecond)
    in
    Cycle (Timeline.Repeat n duration) frames


{-| -}
step : Timeline state -> (state -> Frames sprite) -> sprite
step timeline lookup =
    let
        progress =
            Timeline.progress timeline

        currentFrameSet =
            lookup (current timeline)
    in
    if progress == 1 then
        restFrames currentFrameSet (Timeline.dwellingTime timeline)

    else
        stepFrames currentFrameSet progress


restFrames : Frames item -> Float -> item
restFrames currentFrameSet restingTimeMs =
    case currentFrameSet of
        Frame item ->
            item

        Hold i item ->
            item

        Walk start sprites ->
            let
                index =
                    totalFrames sprites - 1
            in
            getItemAtIndex index (Frame start) 0 sprites

        WithRest (Cycle period cycleFrameList) transitionFrames ->
            let
                len =
                    totalFrames cycleFrameList
            in
            case period of
                Timeline.Loop dur ->
                    let
                        iterationTimeMS =
                            Duration.inMilliseconds dur

                        progress =
                            wrapToUnit (restingTimeMs / iterationTimeMS)

                        targetIndex =
                            floor (progress * toFloat len)
                    in
                    getItemAtIndex targetIndex transitionFrames 0 cycleFrameList

                Timeline.Repeat n dur ->
                    let
                        iterationTimeMS =
                            Duration.inMilliseconds dur

                        iteration =
                            floor (restingTimeMs / iterationTimeMS)

                        progress =
                            if iteration >= n then
                                1

                            else
                                wrapToUnit (restingTimeMs / iterationTimeMS)

                        targetIndex =
                            floor (progress * toFloat len)
                    in
                    getItemAtIndex targetIndex transitionFrames 0 cycleFrameList


stepFrames : Frames item -> Float -> item
stepFrames currentFrameSet progress =
    case currentFrameSet of
        Frame item ->
            item

        Hold i item ->
            item

        Walk start sprites ->
            let
                frameCount =
                    totalFrames sprites

                index =
                    floor (progress * toFloat frameCount) - 1
            in
            getItemAtIndex index (Frame start) 0 sprites

        WithRest _ newFrameSet ->
            stepFrames newFrameSet progress


totalFrames : List (Frames item) -> Int
totalFrames frames =
    List.foldl (\frm total -> total + frameSize frm) 0 frames


frameSize : Frames item -> Int
frameSize myFrame =
    case myFrame of
        Frame _ ->
            1

        Hold i _ ->
            i

        Walk i frames ->
            List.foldl (\frm total -> total + frameSize frm) 0 frames

        WithRest _ newFrameSet ->
            frameSize newFrameSet


getItemAtIndex : Int -> Frames item -> Int -> List (Frames item) -> item
getItemAtIndex targetIndex transitionFrame currentIndex cycleList =
    case cycleList of
        [] ->
            lastFrame transitionFrame

        top :: remain ->
            case top of
                Frame item ->
                    if targetIndex == currentIndex then
                        item

                    else
                        getItemAtIndex targetIndex transitionFrame (currentIndex + 1) remain

                Hold i item ->
                    if currentIndex <= targetIndex && currentIndex + i >= targetIndex then
                        item

                    else
                        getItemAtIndex targetIndex transitionFrame (currentIndex + i) remain

                Walk item allFrames ->
                    let
                        frameCount =
                            totalFrames allFrames
                    in
                    if targetIndex < currentIndex + frameCount then
                        getItemAtIndex targetIndex transitionFrame currentIndex allFrames

                    else
                        getItemAtIndex targetIndex transitionFrame (currentIndex + frameCount) remain

                WithRest _ frames ->
                    let
                        frameCount =
                            frameSize frames
                    in
                    if targetIndex < currentIndex + frameCount then
                        getItemAtIndex targetIndex transitionFrame currentIndex [ frames ]

                    else
                        getItemAtIndex targetIndex transitionFrame (currentIndex + frameCount) remain


lastFrame myFrame =
    case myFrame of
        Frame item ->
            item

        Hold _ item ->
            item

        Walk item remainingFrames ->
            case List.head (List.reverse remainingFrames) of
                Nothing ->
                    item

                Just last ->
                    lastFrame last

        WithRest _ frames ->
            lastFrame frames


{-| -}
subscription : (Timeline state -> msg) -> Timeline state -> Sub msg
subscription toMsg timeline =
    if Timeline.needsUpdate timeline then
        Browser.Events.onAnimationFrame
            (\newTime ->
                toMsg (Timeline.update newTime timeline)
            )

    else
        Sub.none


{-| -}
type alias Animator model =
    Timeline.Animator model



-- Animator (model -> Bool) (Time.Posix -> model -> model)


{-| -}
animator : Animator model
animator =
    Timeline.Animator (always False) (\now model -> model)


{-| -}
with : (model -> Timeline state) -> (Timeline state -> model -> model) -> Animator model -> Animator model
with get set (Timeline.Animator isRunning updateModel) =
    Timeline.Animator
        (\model ->
            if isRunning model then
                True

            else
                Timeline.needsUpdate (get model)
        )
        (\now model ->
            let
                newModel =
                    updateModel now model
            in
            set (Timeline.update now (get newModel)) newModel
        )


{-| -}
toSubscription : (Time.Posix -> msg) -> model -> Animator model -> Sub msg
toSubscription toMsg model (Timeline.Animator isRunning _) =
    if isRunning model then
        Browser.Events.onAnimationFrame
            toMsg

    else
        Sub.none


{-| -}
update : Time.Posix -> Animator model -> model -> model
update newTime (Timeline.Animator _ updateModel) model =
    updateModel newTime model
