module Animator exposing
    ( Timeline, init, subscription
    , Schedule, Event
    , wait, event
    , Duration, millis, seconds, minutes
    , after, between, rewrite
    , queue, interrupt, update
    , float, move, color
    , xy, xyz, to, Movement
    , leave, arrive, linear, smooth, verySmooth
    , leaveLate, arriveEarly
    , loop, wave, wrap, zigzag
    , pause, shift
    )

{-|

@docs Timeline, init, subscription

@docs Schedule, Event

@docs Step, wait, event

@docs Duration, millis, seconds, minutes


# Focusing on Events

@docs after, between, rewrite

@docs queue, interrupt, update


# Animating

@docs float, move, color

@docs xy, xyz, to, Movement

@docs Proportion, leave, arrive, linear, smooth, verySmooth

@docs leaveLate, arriveEarly


# Oscillators

@docs loop, wave, wrap, zigzag

@docs pause, shift

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
type alias Timeline event =
    Timeline.Timeline event


{--}
update =
    Timeline.update


{-| -}
init : Time.Posix -> event -> Timeline event
init start first =
    Timeline.Timeline
        { initial = first
        , now = Time.absolute start
        , events =
            let
                firstOccurring =
                    Timeline.Occurring first (Time.absolute start) Nothing
            in
            Timeline.Timetable
                [ Timeline.Line (Time.absolute start) firstOccurring []
                ]
        , queued = Nothing
        , interruption = []
        , running = True
        }


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


type Step event
    = Wait Duration
    | TransitionTo Duration event


{-| -}
event : Duration -> event -> Step event
event =
    TransitionTo


{-| -}
wait : Duration -> Step event
wait =
    Wait


{-| -}
queue : List (Step event) -> Timeline event -> Timeline event
queue steps (Timeline.Timeline tl) =
    Timeline.Timeline
        { tl
            | queued =
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
interrupt : List (Step event) -> Timeline event -> Timeline event
interrupt steps (Timeline.Timeline tl) =
    Timeline.Timeline
        { tl
            | interruption =
                case initializeSchedule (millis 0) steps of
                    Nothing ->
                        tl.interruption

                    Just ( schedule, otherSteps ) ->
                        List.foldl stepsToEvents schedule otherSteps :: tl.interruption
        }


initializeSchedule : Time.Duration -> List (Step event) -> Maybe ( Schedule event, List (Step event) )
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


stepsToEvents : Step event -> Timeline.Schedule event -> Timeline.Schedule event
stepsToEvents step (Timeline.Schedule delay startEvent events) =
    case events of
        [] ->
            case step of
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
            case step of
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
type alias Event event =
    Timeline.Event event


{-| -}
type alias Schedule event =
    Timeline.Schedule event


{-| -}
rewrite : newEvent -> Timeline event -> (event -> Maybe newEvent) -> Timeline newEvent
rewrite newStart timeline newLookup =
    -- Timeline.rewrite newStart timeline newLookup
    Debug.todo "ugh"


{-| _NOTE_ this might need a rename, it's really "during this even, and after"

So, a timline of `One`, `Two`, `Three`

that calls `Animator.after Two`

would create `False`, `True`, `True`.

-}
after : event -> Timeline event -> Timeline Bool
after =
    Timeline.after


{-| _NOTE_ this might need a rename, it's really "during this even, and after"

So, a timline of `One`, `Two`, `Three`

that calls `Animator.after Two`

would create `False`, `True`, `True`.

-}
between : event -> event -> Timeline event -> Timeline Bool
between =
    Timeline.between



{- Interpolations -}


{-| -}
float : Timeline event -> (event -> Float) -> Float
float timeline lookup =
    .position <|
        move timeline (\ev -> to (lookup ev))


{-| -}
color : Timeline event -> (event -> Color) -> Color
color timeline lookup =
    Timeline.foldp lookup
        Interpolate.startColoring
        Nothing
        Interpolate.color
        timeline


{-| -}
xy : Timeline event -> (event -> { x : Movement, y : Movement }) -> { x : Float, y : Float }
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
xyz : Timeline event -> (event -> { x : Movement, y : Movement, z : Movement }) -> { x : Float, y : Float, z : Float }
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


move : Timeline event -> (event -> Movement) -> { position : Float, velocity : Float }
move timeline lookup =
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
to : Float -> Movement
to =
    Interpolate.Position Interpolate.defaultDeparture Interpolate.defaultArrival



{- PERSONALITY -}


{-| This is an alias for a `Float` between `0` and `1`.

Behind the scenes it will be clamped at those values.

-}
type alias Proportion =
    Float


{-| -}
wobble : Movement -> Movement
wobble movement =
    case movement of
        Interpolate.Position dep arrival pos ->
            Interpolate.Position dep { arrival | wobbliness = 0.8 } pos

        Interpolate.Oscillate dep arrival dur fn ->
            Interpolate.Oscillate dep { arrival | wobbliness = 0.8 } dur fn


{-| -}
withWobble : Proportion -> Movement -> Movement
withWobble p movement =
    case movement of
        Interpolate.Position dep arrival pos ->
            Interpolate.Position dep { arrival | wobbliness = clamp 0 1 p } pos

        Interpolate.Oscillate dep arrival dur fn ->
            Interpolate.Oscillate dep { arrival | wobbliness = clamp 0 1 p } dur fn


{-| -}
linear : Proportion
linear =
    0


{-| -}
smooth : Proportion
smooth =
    0.4


{-| -}
verySmooth : Proportion
verySmooth =
    0.8


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
leave : Proportion -> Movement -> Movement
leave s movement =
    case movement of
        Interpolate.Position dep arrival pos ->
            Interpolate.Position { dep | slowly = clamp 0 1 s } arrival pos

        Interpolate.Oscillate dep arrival dur fn ->
            Interpolate.Oscillate { dep | slowly = clamp 0 1 s } arrival dur fn


{-| -}
arrive : Proportion -> Movement -> Movement
arrive s movement =
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
within tolerance anchor at =
    let
        low =
            anchor - tolerance

        high =
            anchor + tolerance
    in
    at >= low && at <= high


pauseToBounds : Pause -> Duration -> Duration -> ( Float, Float )
pauseToBounds (Pause dur at) activeDuration totalDur =
    let
        start =
            Quantity.multiplyBy at activeDuration
    in
    ( Quantity.ratio start totalDur
    , Quantity.ratio (Quantity.plus start dur) totalDur
    )


pauseValue : Pause -> Float
pauseValue (Pause _ v) =
    v


{-| -}
loop : Duration -> Oscillator -> Movement
loop activeDuration (Oscillator pauses osc) =
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
    Interpolate.Oscillate Interpolate.defaultDeparture
        Interpolate.defaultArrival
        totalDuration
        fn


{-| Shift an oscillator over by a certain amount.

It's expecting a number between 0 and 1.

-}
shift : Float -> Oscillator -> Oscillator
shift x (Oscillator pauses osc) =
    Oscillator
        pauses
        (\u -> osc (wrapToUnit (u + x)))


wrapToUnit x =
    x - toFloat (floor x)


{-| When the oscillator is at a certain point, pause.

This pause time will be added to the time you specify using `loop`, so that you can adjust the pause without disturbing the original duration of the oscillator.

-}
pause : Duration -> Float -> Oscillator -> Oscillator
pause forDuration at (Oscillator pauses osc) =
    Oscillator
        (Pause forDuration at :: pauses)
        osc


orbit : { duration : Duration, toPosition : Float -> Float } -> Movement
orbit config =
    Interpolate.Oscillate
        Interpolate.defaultDeparture
        Interpolate.defaultArrival
        config.duration
        config.toPosition


{-| Start at one number and move linearly to another. At th end, wrap to the first.
-}
wrap : Float -> Float -> Oscillator
wrap start end =
    let
        total =
            end - start
    in
    Oscillator []
        (\u ->
            u * total
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


{-| -}
zigzag : Float -> Float -> Oscillator
zigzag start end =
    let
        total =
            end - start
    in
    Oscillator []
        (\u ->
            start + (total * u)
        )



{-

   Fade
      -> target opacity
      ->

   Color
       -> target color

   Rotation
       -> Target angle
       -> Target speed + direction
       -> Target origin + axis

   Position
       -> Target position
       -> Oscillator
           |> every (8 seconds)
               (0-1 -> position)

       -> Wiggle
           |> {pos, velocity, direction, progress: 0-1, durationSinceStart : Time}
                   -> Delta position
                       (possibly informed )
       ->

   MotionBlur

       |> (velocity -> Blur value)


   Scale
       -> Target Scale

-}


{-| -}
subscription : (Timeline event -> msg) -> Timeline event -> Sub msg
subscription toMsg timeline =
    if Timeline.needsUpdate timeline then
        Browser.Events.onAnimationFrame
            (\newTime ->
                toMsg (Timeline.update newTime timeline)
            )

    else
        Sub.none
