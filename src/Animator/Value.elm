module Animator.Value exposing
    ( color
    , float, velocity, movement, Movement, to, xy, xyz
    , withTransition
    )

{-| You may want to animate a value manually, without generating any CSS.

This module is for you!

You'll need to track a `Timeline` in your model and update it using `Browser.Events.onAnimationFrame`.

@docs color

@docs float, velocity, movement, Movement, to, xy, xyz

@docs withTransition

-}

import Animator.Timeline exposing (Timeline)
import Animator.Transition
import Color exposing (Color)
import InternalAnim.Move as Move
import InternalAnim.Quantity as Quantity
import InternalAnim.Time as Time
import InternalAnim.Timeline as Timeline



{- Interpolations -}


{-| -}
type alias Movement =
    Move.Move Float


{-| -}
color : Timeline state -> (state -> Color) -> Color
color timeline lookup =
    Timeline.foldpAll (Timeline.getCurrentTime timeline)
        lookup
        identity
        (\_ target now startTime endTime _ state ->
            if Time.thisAfterOrEqualThat now startTime then
                let
                    targetTime =
                        Timeline.startTime target

                    progress =
                        Time.progress startTime targetTime (sampleTime now endTime)
                in
                Move.lerpColor progress state (lookup (Timeline.getEvent target))

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


{-| Units per second: if your values are pixels, this returns pixels per second.
-}
velocity : Timeline state -> (state -> Movement) -> Float
velocity timeline lookup =
    movement timeline lookup
        |> .velocity


{-| The interpolated position and velocity (in value units per second).
Completed transitions hold their destination with zero velocity. Interrupted
transitions contribute their position and velocity at the interruption time.
-}
movement : Timeline state -> (state -> Movement) -> { position : Float, velocity : Float }
movement timeline lookup =
    Timeline.foldpAll (Timeline.getCurrentTime timeline)
        lookup
        Move.init
        (\_ target now startTransition interruptedOrEnd _ state ->
            if Time.thisAfterOrEqualThat now startTransition then
                let
                    arrived =
                        Timeline.startTime target

                    progress =
                        Time.progress startTransition arrived (sampleTime now interruptedOrEnd)

                    targetMovement =
                        lookup (Timeline.getEvent target)
                in
                Move.at progress
                    startTransition
                    arrived
                    targetMovement
                    state

            else
                state
        )
        timeline
        |> unwrapUnits


sampleTime : Time.Absolute -> Time.Absolute -> Time.Absolute
sampleTime now end =
    if Time.thisBeforeThat end now then
        end

    else
        now


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
        float timeline (lookup >> .y)
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


{-| -}
withTransition : Animator.Transition.Transition -> Movement -> Movement
withTransition =
    Move.withTransition



-- {-| Leave a state with some initial velocity.
-- This is given as a velocity (as value/second). Usually this is pixels per second, but depends what you're animating.
--   - `withImpulse 0` - No initial velocity (the default)
--   - `withImpulse 200` - 200 units per second towards
--   - `withImpulse -200` - Negative values work too!
-- -}
-- withImpulse : Float -> Movement -> Movement
-- withImpulse p movement =
--     Debug.todo "Move to Transitions"
