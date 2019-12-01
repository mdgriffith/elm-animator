module Animator exposing
    ( Timeline, init, subscription
    , Schedule, Event
    , wait, event
    , Duration, millis, seconds, minutes
    , after, between, rewrite
    , queue, update
    , float, color
    , move, to, orbit
    )

{-|

@docs Timeline, init, subscription

@docs Schedule, Event

@docs Step, wait, event

@docs Duration, millis, seconds, minutes


# Focusing on Events

@docs after, between, rewrite

@docs queue, update


# Animating

@docs float, color

@docs move, xy, xyz, to, orbit

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


init : Time.Posix -> event -> Timeline event
init start first =
    Timeline.Timeline
        { initial = first
        , now = Time.absolute start
        , events = [ Timeline.Occurring first (Time.absolute start) Nothing ]
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


stepsToEvents : Step event -> Timeline.Schedule event -> Timeline.Schedule event
stepsToEvents step (Timeline.Schedule delay events) =
    case events of
        [] ->
            case step of
                Wait waiting ->
                    Timeline.Schedule
                        (Quantity.plus delay waiting)
                        events

                TransitionTo dur checkpoint ->
                    Timeline.Schedule
                        delay
                        [ Timeline.Event dur checkpoint Nothing ]

        (Timeline.Event durationTo recentEvent maybeDwell) :: remaining ->
            case step of
                Wait dur ->
                    Timeline.Schedule
                        delay
                        (Timeline.Event durationTo recentEvent (addToDwell dur maybeDwell) :: remaining)

                TransitionTo dur checkpoint ->
                    if checkpoint == recentEvent then
                        Timeline.Schedule
                            delay
                            (Timeline.Event durationTo recentEvent (addToDwell dur maybeDwell) :: remaining)

                    else
                        Timeline.Schedule
                            delay
                            (Timeline.Event dur checkpoint Nothing :: events)


addToDwell duration maybeDwell =
    case maybeDwell of
        Nothing ->
            Just duration

        Just existing ->
            Just (Quantity.plus duration existing)


queue : List (Step event) -> Timeline event -> Timeline event
queue steps (Timeline.Timeline tl) =
    Timeline.Timeline
        { tl
            | queued =
                Just (List.foldl stepsToEvents (Timeline.Schedule (millis 0) []) steps)
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


{-| _NOTE_ this might need a rename, it's really "during this even, and after"

So, a timline of `One`, `Two`, `Three`

that calls `Animator.after Two`

would create `False`, `True`, `True`.

-}
after : event -> Timeline event -> Timeline Bool
after ev timeline =
    Timeline.after ev timeline


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
            Interpolate.xyz
            timeline


move : Timeline event -> (event -> Movement) -> { position : Float, velocity : Float }
move timeline lookup =
    unwrapUnits
        (Timeline.foldp lookup
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


type alias Movement =
    Interpolate.Movement


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
