module Animator exposing
    ( Schedule, Event
    , Timeline, init, subscription, rewrite
    , queue, update
    , float, motion, color
    , move, to, orbit
    , moveMotion
    , wait, event
    , Duration, millis, seconds, minutes
    )

{-|

@docs Schedule, Event

@docs Timeline, init, subscription, rewrite

@docs queue, update

@docs float, motion, color

@docs move, to, orbit
@docs moveMotion

@docs Step, wait, event

@docs Duration, millis, seconds, minutes

-}

import Browser.Events
import Color exposing (Color)
import Duration
import Internal.Interpolate as Interpolate
import Internal.Time as Time
import Internal.Timeline as Timeline
import Quantity


{-| -}
type alias Timeline event =
    Timeline.Timeline event


{--}
update =
    Timeline.update


init : event -> Timeline event
init first =
    Timeline.Timeline
        { initial = first
        , start = Quantity.Quantity 0
        , now = Quantity.Quantity 0
        , events = []
        , queued = Nothing
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


stepsToEvents step ( waiting, events ) =
    case events of
        [] ->
            case step of
                Wait dur ->
                    ( Quantity.plus waiting dur
                    , events
                    )

                TransitionTo dur checkpoint ->
                    ( millis 0
                    , [ Timeline.Event (Quantity.plus waiting dur) checkpoint ]
                    )

        (Timeline.Event durationTo recentEvent) :: remaining ->
            case step of
                Wait dur ->
                    ( millis 0
                    , Timeline.Event (Quantity.plus waiting dur) recentEvent :: events
                    )

                TransitionTo dur checkpoint ->
                    ( millis 0
                    , Timeline.Event (Quantity.plus waiting dur) checkpoint :: events
                    )


queue : List (Step event) -> Timeline event -> Timeline event
queue steps (Timeline.Timeline tl) =
    let
        events =
            List.foldl stepsToEvents ( millis 0, [] ) steps
                |> Tuple.second
                |> List.reverse
    in
    Timeline.Timeline
        { tl
            | queued =
                Just (Timeline.Schedule events)
        }


{-| -}
type alias Event event =
    Timeline.Event event


{-| -}
type alias Schedule event =
    Timeline.Schedule event


{-| -}
rewrite : newEvent -> Timeline event -> (event -> Maybe newEvent) -> Timeline newEvent
rewrite newStart timeline newLookup =
    Timeline.rewrite newStart timeline newLookup



{- Interpolations -}


{-| -}
float : Timeline event -> (event -> Float) -> Float
float timeline lookup =
    .position <|
        Timeline.foldp lookup
            Interpolate.toMotion
            Interpolate.motion
            timeline


{-| -}
color : Timeline event -> (event -> Color) -> Color
color timeline lookup =
    Timeline.foldp lookup
        Interpolate.linear
        Interpolate.color
        timeline


{-| -}
motion : Timeline event -> (event -> Float) -> Interpolate.Motion
motion timeline lookup =
    Timeline.foldp lookup
        Interpolate.toMotion
        Interpolate.motion
        timeline


{-| -}
position : Timeline event -> (event -> { x : Float, y : Float }) -> { x : Float, y : Float }
position timeline lookup =
    (\{ x, y } ->
        { x = x.position
        , y = y.position
        }
    )
    <|
        Timeline.foldp lookup
            Interpolate.toPoint
            Interpolate.point
            timeline


type alias Movement =
    Interpolate.Movement


move : Timeline event -> (event -> Movement) -> Float
move timeline lookup =
    .position <|
        Timeline.foldp lookup
            Interpolate.toMovement
            Interpolate.movement
            timeline


moveMotion : Timeline event -> (event -> Movement) -> Interpolate.MotionMovement
moveMotion timeline lookup =
    Timeline.foldp lookup
        Interpolate.toMovement
        Interpolate.movement
        timeline


to : Float -> Movement
to =
    Interpolate.Position


orbit : { duration : Duration, point : Float, toPosition : Float -> Float } -> Movement
orbit config =
    Interpolate.Oscillate config.point config.duration config.toPosition



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
