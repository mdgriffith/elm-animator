module Animator exposing
    ( Timeline, init
    , current
    , Animator, animator, with, toSubscription
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
    , Item, through, cycle, cycleN
    , Frame, frame, hold
    )

{-|

@docs Timeline, init

@docs current


# Animating

@docs Animator, animator, with, toSubscription


# Adding events to a timeline

@docs to

@docs Duration, millis, seconds, minutes

@docs Step, wait, event, interrupt, queue


# Animating

@docs color

@docs linear, float

@docs Movement, move, xy, xyz

@docs details


# Crafting Movement

@docs at

@docs Proportion

@docs leaveSmoothly, leaveLate

@docs arriveSmoothly, arriveEarly

@docs withWobble


# Oscillators

@docs wave, wrap, zigzag

@docs loop, repeat, once

@docs pause, shift


# Sprites

@docs step

@docs Item, through, cycle, cycleN

@docs Frame, frame, hold

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
current : Timeline state -> state
current =
    Timeline.getEvent << Timeline.current


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
describe : Timeline state -> List (Description state)
describe timeline =
    Timeline.foldp identity
        Interpolate.startDescription
        Nothing
        Interpolate.describe
        timeline


{-| -}
color : Timeline state -> (state -> Color) -> Color
color timeline lookup =
    Timeline.foldp lookup
        Interpolate.startColoring
        Nothing
        Interpolate.color
        timeline


{-| Interpolate a float linearly between destinations.
-}
linear : Timeline state -> (state -> Float) -> Float
linear timeline lookup =
    Timeline.foldp lookup
        Interpolate.startLinear
        Nothing
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
    (\{ x, y } ->
        { x = unwrapUnits x |> .position
        , y = unwrapUnits y |> .position
        }
    )
    <|
        Timeline.foldp lookup
            Interpolate.startMovingXy
            Nothing
            Interpolate.xy
            timeline


{-| -}
xyz : Timeline state -> (state -> { x : Movement, y : Movement, z : Movement }) -> { x : Float, y : Float, z : Float }
xyz timeline lookup =
    (\{ x, y, z } ->
        { x = unwrapUnits x |> .position
        , y = unwrapUnits y |> .position
        , z = unwrapUnits z |> .position
        }
    )
    <|
        Timeline.foldp lookup
            Interpolate.startMovingXyz
            Nothing
            Interpolate.xyz
            timeline


{-| -}
details : Timeline state -> (state -> Movement) -> { position : Float, velocity : Float }
details timeline lookup =
    unwrapUnits
        (Timeline.foldp lookup
            Interpolate.startMoving
            (Just Interpolate.adjustTiming)
            Interpolate.move
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
        (Interpolate.Repeat 1 totalDuration)
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
        (Interpolate.Loop totalDuration)
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
        (Interpolate.Repeat n totalDuration)
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


type Item item
    = Through (List (Frame item))
    | Cycle Interpolate.Period (List (Frame item))


type Frame item
    = Frame item
    | FramePause Int item


{-| -}
step : Timeline state -> sprite -> (state -> Item sprite) -> sprite
step timeline defaultSprite lookup =
    case lookup (current timeline) of
        Through sprites ->
            let
                progress =
                    Timeline.progress timeline

                len =
                    List.length sprites

                index =
                    floor (progress * toFloat len)
            in
            getItemAtIndex index defaultSprite 0 sprites

        Cycle period sprites ->
            let
                totalMS =
                    Timeline.dwellingTime timeline

                len =
                    List.length sprites
            in
            case period of
                Interpolate.Loop dur ->
                    let
                        iterationTimeMS =
                            Duration.inMilliseconds dur

                        progress =
                            wrapToUnit (totalMS / iterationTimeMS)

                        index =
                            floor (progress * toFloat len)
                    in
                    getItemAtIndex index defaultSprite 0 sprites

                Interpolate.Repeat n dur ->
                    let
                        iterationTimeMS =
                            Duration.inMilliseconds dur

                        iteration =
                            floor (totalMS / iterationTimeMS)

                        progress =
                            if iteration >= n then
                                1

                            else
                                wrapToUnit (totalMS / iterationTimeMS)

                        index =
                            floor (progress * toFloat len)
                    in
                    getItemAtIndex index defaultSprite 0 sprites


getItemAtIndex : Int -> item -> Int -> List (Frame item) -> item
getItemAtIndex targetIndex default currentIndex list =
    case list of
        [] ->
            default

        top :: remain ->
            case top of
                Frame item ->
                    if targetIndex == currentIndex then
                        item

                    else
                        getItemAtIndex targetIndex default (currentIndex + 1) remain

                FramePause i item ->
                    if currentIndex <= targetIndex && currentIndex + i >= targetIndex then
                        item

                    else
                        getItemAtIndex targetIndex default (currentIndex + i) remain


{-| -}
frame : sprite -> Frame sprite
frame =
    Frame


{-| -}
hold : Int -> sprite -> Frame sprite
hold =
    FramePause


{-| -}
through : List (Frame sprite) -> Item sprite
through =
    Through


{-| -}
cycle : Duration -> List (Frame sprite) -> Item sprite
cycle duration frames =
    Cycle (Interpolate.Loop duration) frames


{-| -}
cycleN : Int -> Duration -> List (Frame sprite) -> Item sprite
cycleN n duration frames =
    Cycle (Interpolate.Repeat n duration) frames


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
type Animator model msg
    = Animator (model -> Bool) (Time.Posix -> model -> model) (model -> msg)


{-| -}
animator : (model -> msg) -> Animator model msg
animator toMsg =
    Animator (always False) (\now model -> model) toMsg


{-| -}
with : (model -> Timeline state) -> (Timeline state -> model -> model) -> Animator model msg -> Animator model msg
with get set (Animator isRunning updateModel toMsg) =
    Animator
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
        toMsg


{-| -}
toSubscription : model -> Animator model msg -> Sub msg
toSubscription model (Animator isRunning updateModel toMsg) =
    if isRunning model then
        Browser.Events.onAnimationFrame
            (\newTime ->
                toMsg (updateModel newTime model)
            )

    else
        Sub.none
