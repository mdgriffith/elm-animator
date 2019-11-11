module Animator exposing
    ( Schedule, Event
    , Timeline, init, subscription, rewrite
    , queue
    , float, motion, color
    , update
    )

{-|

@docs Schedule, Event

@docs Timeline, init, subscription, rewrite

@docs queue

@docs float, motion, color

-}

import Browser.Events
import Color exposing (Color)
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


queue : List ( Float, event ) -> Timeline event -> Timeline event
queue events (Timeline.Timeline tl) =
    Timeline.Timeline
        { tl
            | queued =
                Just (toSchedule events)
        }


toSchedule events =
    Timeline.Schedule
        (List.map
            (\( time, ev ) ->
                Timeline.Event (Time.durationInMs time) ev
            )
            events
        )


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


type Movement
    = Oscillate Time.Duration (Float -> { x : Float, y : Float })
    | Position { x : Float, y : Float }


move : Timeline event -> (event -> Movement) -> { x : Float, y : Float }
move timeline lookup =
    { x = 0, y = 0 }



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
