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
import Internal.Timeline as Timeline
import Quantity



{- Interpolations -}


{-| -}
color : Timeline state -> (state -> Color) -> Color
color timeline lookup =
    Timeline.foldp
        lookup
        Interpolate.coloring
        timeline


{-| -}
move : Timeline state -> (state -> Movement) -> Float
move timeline lookup =
    .position <|
        Interpolate.details timeline lookup


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
    Move.to



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
