module Animator.Value exposing
    ( color
    , float, velocity, movement, Movement, to, xy, xyz
    , withWobble
    )

{-|

@docs color

@docs float, velocity, movement, Movement, to, xy, xyz


# Transition personality

@docs withWobble

-}

import Animator.Timeline exposing (Timeline)
import Color exposing (Color)
import Internal.Move as Move
import Internal.Time as Time
import Internal.Timeline as Timeline
import Quantity



{- Interpolations -}


{-| -}
type alias Movement =
    Move.Move Float


{-| -}
color : Timeline state -> (state -> Color) -> Color
color timeline lookup =
    Timeline.foldpAll lookup
        identity
        (\_ prev target now startTime endTime future state ->
            let
                isHappening =
                    (Time.thisAfterOrEqualThat now startTime
                        && Time.thisBeforeOrEqualThat now endTime
                    )
                        || List.isEmpty future
                        && Time.thisAfterThat now endTime
            in
            if isHappening then
                let
                    targetTime =
                        Timeline.startTime target

                    progress =
                        Time.progress startTime targetTime now
                in
                Move.color progress
                    (lookup (Timeline.getEvent prev))
                    (lookup (Timeline.getEvent target))

            else
                state
        )
        timeline


{-| -}
to : Float -> Movement
to =
    Move.to


{-| -}
float : Timeline state -> (state -> Movement) -> Float
float timeline lookup =
    movement timeline lookup
        |> .position


{-| -}
velocity : Timeline state -> (state -> Movement) -> Float
velocity timeline lookup =
    movement timeline lookup
        |> .velocity


movement : Timeline state -> (state -> Movement) -> { position : Float, velocity : Float }
movement timeline lookup =
    Timeline.foldpAll lookup
        Move.init
        (\_ prev target now startTransition interruptedOrEnd future state ->
            let
                arrived =
                    Timeline.startTime target

                isHappening =
                    (Time.thisAfterOrEqualThat now startTransition
                        && Time.thisBeforeOrEqualThat now arrived
                    )
                        || (List.isEmpty future
                                && Time.thisAfterThat now interruptedOrEnd
                           )
            in
            if isHappening then
                let
                    progress =
                        Time.progress startTransition arrived now

                    targetMovement =
                        lookup (Timeline.getEvent target)
                in
                Move.transitionTo progress
                    startTransition
                    arrived
                    targetMovement
                    state

            else
                state
        )
        timeline
        |> unwrapUnits


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
        float timeline (lookup >> .x)
    , y =
        float timeline (lookup >> .y)
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
        float timeline (lookup >> .x)
    , y =
        float timeline (lookup >> .x)
    , z =
        float timeline (lookup >> .z)
    }


unwrapUnits : Move.State -> { position : Float, velocity : Float }
unwrapUnits state =
    { position =
        case state.position of
            Quantity.Quantity val ->
                val
    , velocity =
        case state.velocity of
            Quantity.Quantity val ->
                val
    }



{- PERSONALITY -}


{-| This will make the transition use a spring!

  - `withWobble 0` - absolutely no wobble
  - `withWobble 1` - all the wobble

-}
withWobble : Float -> Movement -> Movement
withWobble =
    Move.withWobble



-- {-| Leave a state with some initial velocity.
-- This is given as a velocity (as value/second). Usually this is pixels per second, but depends what you're animating.
--   - `withImpulse 0` - No initial velocity (the default)
--   - `withImpulse 200` - 200 units per second towards
--   - `withImpulse -200` - Negative values work too!
-- -}
-- withImpulse : Float -> Movement -> Movement
-- withImpulse p movement =
--     Debug.todo "Move to Transitions"
