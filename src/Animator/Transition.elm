module Animator.Transition exposing
    ( Transition
    , linear, standard
    , wobble, bezier
    )

{-|

@docs Transition

@docs linear, standard

@docs wobble, bezier

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
wobble : Float -> Transition
wobble =
    InternalAnim.Transition.wobble


{-| -}
bezier : Float -> Float -> Float -> Float -> Transition
bezier =
    InternalAnim.Transition.bezier
