module Animator.Transition exposing
    ( Transition
    , linear, standard
    , spring, bezier
    )

{-|

@docs Transition

@docs linear, standard

@docs spring, bezier

-}

import InternalAnim.Transition


{-| -}
type alias Transition =
    InternalAnim.Transition.Transition


{-| -}
linear : Transition
linear =
    InternalAnim.Transition.linear


{-| The default easing: `cubic-bezier(0.4, 0, 0.2, 1)`.
-}
standard : Transition
standard =
    InternalAnim.Transition.standard


{-| Both parameters range from `0` to `1`; values outside that range are clamped.

  - `wobble`: how much the motion bounces around its destination. `0` is the least
    wobbly setting, `1` the most. Even `0` can overshoot, especially at short durations.
  - `quickness`: how snappy the spring feels. `0` is softer, `1` is stiffer.

Start here, then adjust to taste:

    import Animator
    import Animator.Transition as Transition

    Animator.transition (Animator.ms 600)
        [ Animator.x 200
            |> Animator.withTransition
                (Transition.spring { wobble = 0.4, quickness = 0.5 })
        ]

Duration still comes from the animation (`600ms` above). `quickness` is not a
duration multiplier. Short durations can make the spring feel different, so tune
these settings at the duration you intend to use.

-}
spring : { wobble : Float, quickness : Float } -> Transition
spring =
    InternalAnim.Transition.wobble


{-| Arguments follow CSS `cubic-bezier(x1, y1, x2, y2)` order.
Keep X coordinates in `0..1`. Y coordinates may go outside that range for overshoot.
-}
bezier : Float -> Float -> Float -> Float -> Transition
bezier =
    InternalAnim.Transition.bezier
