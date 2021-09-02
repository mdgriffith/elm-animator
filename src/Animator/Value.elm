module Animator.Value exposing
    ( color
    , Movement, at, move, xy, xyz
    , withWobble, withImpulse
    )

{-|

@docs color

@docs Movement, at, move, xy, xyz


# Transition personality

@docs withWobble, withImpulse

-}

import Animator.Timeline exposing (Timeline)
import Color exposing (Color)
import Internal.Interpolate as Interpolate
import Internal.Move as Move
import Internal.Time as Time
import Internal.Timeline as Timeline
import Pixels
import Quantity



{- Interpolations -}


{-| -}
type alias Movement =
    Interpolate.Movement


{-| -}
color : Timeline state -> (state -> Color) -> Color
color timeline lookup =
    Timeline.foldpAll lookup
        identity
        (\_ prev target now startTime endTime future state ->
            let
                targetTime =
                    Timeline.startTime target

                progress =
                    Time.progress startTime targetTime now

                movement =
                    lookup (Timeline.getEvent target)
            in
            Interpolate.color progress
                (lookup (Timeline.getEvent prev))
                (lookup (Timeline.getEvent target))
        )
        timeline


{-| -}
at : Float -> Movement
at =
    Move.to


float : Timeline state -> (state -> Float) -> Float
float timeline lookup =
    move timeline
        (lookup >> at)


{-| -}
move : Timeline state -> (state -> Movement) -> Float
move timeline lookup =
    Timeline.foldpAll lookup
        Move.init
        (\_ prev target now startTime endTime future state ->
            let
                targetTime =
                    Timeline.startTime target

                progress =
                    Time.progress startTime targetTime now

                movement =
                    lookup (Timeline.getEvent target)
            in
            Move.transitionTo progress
                startTime
                targetTime
                movement
                state
        )
        timeline
        |> unwrapUnits
        |> .position


{-| -}
xy :
    Timeline state
    ->
        (state
         ->
            { x : Movement
            , y : Movement
            }
        )
    -> { x : Float, y : Float }
xy timeline lookup =
    { x =
        move timeline (lookup >> .x)
    , y =
        move timeline (lookup >> .y)
    }


{-| -}
xyz :
    Timeline state
    ->
        (state
         ->
            { x : Movement
            , y : Movement
            , z : Movement
            }
        )
    -> { x : Float, y : Float, z : Float }
xyz timeline lookup =
    { x =
        move timeline (lookup >> .x)
    , y =
        move timeline (lookup >> .x)
    , z =
        move timeline (lookup >> .z)
    }


unwrapUnits : Move.State -> { position : Float, velocity : Float }
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



{- PERSONALITY -}


{-| This will make the transition use a spring!

  - `withWobble 0` - absolutely no wobble
  - `withWobble 1` - all the wobble

-}
withWobble : Float -> Movement -> Movement
withWobble w movement =
    Debug.todo "Move to Transitions"


{-| Leave a state with some initial velocity.

This is given as a velocity (as value/second). Usually this is pixels per second, but depends what you're animating.

  - `withImpulse 0` - No initial velocity (the default)
  - `withImpulse 200` - 200 units per second towards
  - `withImpulse -200` - Negative values work too!

-}
withImpulse : Float -> Movement -> Movement
withImpulse p movement =
    Debug.todo "Move to Transitions"
