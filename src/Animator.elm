module Animator exposing
    ( Timeline, init, initWith
    , current
    , Animator, animator, with, toSubscription, update
    , to
    , Duration, millis, seconds, minutes
    , interrupt, queue, Step, wait, event
    , color
    , linear, float
    , Movement, move, xy, xyz
    , details
    , at
    , Proportion
    , leaveSmoothly, leaveLate
    , arriveSmoothly, arriveEarly
    , withWobble
    , Oscillator, wave, wrap, zigzag, interpolate
    , loop, once, repeat
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

In some cases you might want to define a series of states to animate through.

In that case, you'll want to define a list of steps and either interrupt what's currently happening with them or queue them up.

@docs interrupt, queue, Step, wait, event


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

@docs Oscillator, wave, wrap, zigzag, interpolate

Once we've created an oscillator, we need to specify how long it should take and how many times it should repeat.

@docs loop, once, repeat

Adjust an oscillator by adding pauses or shifting it.

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
    Timeline.foldp
        lookup
        Interpolate.coloring
        timeline


{-| Interpolate a float linearly between destinations.
-}
linear : Timeline state -> (state -> Float) -> Float
linear timeline lookup =
    Timeline.foldp
        lookup
        Interpolate.linearly
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
        Timeline.foldp
            (lookup >> .x)
            Interpolate.moving
            timeline
            |> unwrapUnits
            |> .position
    , y =
        Timeline.foldp
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
        Timeline.foldp
            (lookup >> .x)
            Interpolate.moving
            timeline
            |> unwrapUnits
            |> .position
    , y =
        Timeline.foldp
            (lookup >> .y)
            Interpolate.moving
            timeline
            |> unwrapUnits
            |> .position
    , z =
        Timeline.foldp
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
        (Timeline.foldp
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
type alias Oscillator =
    Timeline.Oscillator


{-| -}
type alias Pause =
    Timeline.Pause


within : Float -> Float -> Float -> Bool
within tolerance anchor val =
    let
        low =
            anchor - tolerance

        high =
            anchor + tolerance
    in
    val >= low && val <= high


{-| -}
once : Duration -> Oscillator -> Movement
once activeDuration osc =
    case osc of
        Timeline.Resting i ->
            at i

        Timeline.Oscillator pauses fn ->
            let
                ( preparedFn, totalDuration ) =
                    Timeline.prepareOscillator activeDuration pauses fn
            in
            Interpolate.Oscillate Interpolate.defaultDeparture
                Interpolate.defaultArrival
                (Timeline.Repeat 1 totalDuration)
                preparedFn


{-| -}
loop : Duration -> Oscillator -> Movement
loop activeDuration osc =
    case osc of
        Timeline.Resting i ->
            at i

        Timeline.Oscillator pauses fn ->
            let
                ( preparedFn, totalDuration ) =
                    Timeline.prepareOscillator activeDuration pauses fn
            in
            Interpolate.Oscillate Interpolate.defaultDeparture
                Interpolate.defaultArrival
                (Timeline.Loop totalDuration)
                preparedFn


{-| -}
repeat : Int -> Duration -> Oscillator -> Movement
repeat n activeDuration osc =
    case osc of
        Timeline.Resting i ->
            at i

        Timeline.Oscillator pauses fn ->
            let
                ( preparedFn, totalDuration ) =
                    Timeline.prepareOscillator activeDuration pauses fn
            in
            Interpolate.Oscillate Interpolate.defaultDeparture
                Interpolate.defaultArrival
                (Timeline.Repeat n totalDuration)
                preparedFn


{-| Shift an oscillator over by a certain amount.

It's expecting a number between 0 and 1.

-}
shift : Proportion -> Oscillator -> Oscillator
shift x osc =
    case osc of
        Timeline.Oscillator pauses fn ->
            Timeline.Oscillator
                pauses
                (\u -> fn (wrapToUnit (u + x)))

        Timeline.Resting _ ->
            osc


wrapToUnit : Float -> Float
wrapToUnit x =
    x - toFloat (floor x)


{-| Pause the the oscillator is at a certain point.

This pause time will be added to the time you specify using `loop`, so that you can adjust the pause without disturbing the original duration of the oscillator.

-}
pause : Duration -> Proportion -> Oscillator -> Oscillator
pause forDuration val osc =
    case osc of
        Timeline.Oscillator pauses fn ->
            Timeline.Oscillator
                (Timeline.Pause forDuration val :: pauses)
                fn

        Timeline.Resting _ ->
            osc


{-| Start at one number and move linearly to another, then wrap back to the first.
-}
wrap : Float -> Float -> Oscillator
wrap start end =
    let
        total =
            end - start
    in
    Timeline.Oscillator []
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
    Timeline.Oscillator []
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
    Timeline.Oscillator []
        (\u ->
            start + total * (1 - abs (2 * u - 1))
        )


{-| -}
interpolate : (Proportion -> Float) -> Oscillator
interpolate interp =
    Timeline.Oscillator [] interp



{- SPRITES -}


{-| -}
type alias Frames item =
    Timeline.Frames item


{-| -}
type alias Resting item =
    Timeline.Resting item


{-| -}
frame : sprite -> Frames sprite
frame =
    Timeline.Single


{-| -}
hold : Int -> sprite -> Frames sprite
hold =
    Timeline.Hold


{-| -}
walk : sprite -> List (Frames sprite) -> Frames sprite
walk =
    Timeline.Walk


{-| -}
framesWith :
    { transition : Frames item
    , resting : Resting item
    }
    -> Frames item
framesWith cfg =
    Timeline.WithRest
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
    Timeline.Cycle (Timeline.Loop duration) frames


{-| -}
cycleN : Int -> FramesPerSecond -> List (Frames sprite) -> Resting sprite
cycleN n (FramesPerSecond framesPerSecond) frames =
    let
        duration =
            Duration.seconds (toFloat (List.length frames) / framesPerSecond)
    in
    Timeline.Cycle (Timeline.Repeat n duration) frames


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
        Timeline.Single item ->
            item

        Timeline.Hold i item ->
            item

        Timeline.Walk start sprites ->
            let
                index =
                    totalFrames sprites - 1
            in
            getItemAtIndex index (Timeline.Single start) 0 sprites

        Timeline.WithRest (Timeline.Cycle period cycleFrameList) transitionFrames ->
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
        Timeline.Single item ->
            item

        Timeline.Hold i item ->
            item

        Timeline.Walk start sprites ->
            let
                frameCount =
                    totalFrames sprites

                index =
                    floor (progress * toFloat frameCount) - 1
            in
            getItemAtIndex index (Timeline.Single start) 0 sprites

        Timeline.WithRest _ newFrameSet ->
            stepFrames newFrameSet progress


totalFrames : List (Frames item) -> Int
totalFrames frames =
    List.foldl (\frm total -> total + frameSize frm) 0 frames


frameSize : Frames item -> Int
frameSize myFrame =
    case myFrame of
        Timeline.Single _ ->
            1

        Timeline.Hold i _ ->
            i

        Timeline.Walk i frames ->
            List.foldl (\frm total -> total + frameSize frm) 0 frames

        Timeline.WithRest _ newFrameSet ->
            frameSize newFrameSet


getItemAtIndex : Int -> Frames item -> Int -> List (Frames item) -> item
getItemAtIndex targetIndex transitionFrame currentIndex cycleList =
    case cycleList of
        [] ->
            lastFrame transitionFrame

        top :: remain ->
            case top of
                Timeline.Single item ->
                    if targetIndex == currentIndex then
                        item

                    else
                        getItemAtIndex targetIndex transitionFrame (currentIndex + 1) remain

                Timeline.Hold i item ->
                    if currentIndex <= targetIndex && currentIndex + i >= targetIndex then
                        item

                    else
                        getItemAtIndex targetIndex transitionFrame (currentIndex + i) remain

                Timeline.Walk item allFrames ->
                    let
                        frameCount =
                            totalFrames allFrames
                    in
                    if targetIndex < currentIndex + frameCount then
                        getItemAtIndex targetIndex transitionFrame currentIndex allFrames

                    else
                        getItemAtIndex targetIndex transitionFrame (currentIndex + frameCount) remain

                Timeline.WithRest _ frames ->
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
        Timeline.Single item ->
            item

        Timeline.Hold _ item ->
            item

        Timeline.Walk item remainingFrames ->
            case List.head (List.reverse remainingFrames) of
                Nothing ->
                    item

                Just last ->
                    lastFrame last

        Timeline.WithRest _ frames ->
            lastFrame frames


{-| -}
type alias Animator model =
    Timeline.Animator model


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
