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


{-| -}
standard : Transition
standard =
    InternalAnim.Transition.standard


{-| -}
spring : { wobble : Float, quickness : Float } -> Transition
spring =
    InternalAnim.Transition.wobble


{-| -}
bezier : Float -> Float -> Float -> Float -> Transition
bezier =
    InternalAnim.Transition.bezier
