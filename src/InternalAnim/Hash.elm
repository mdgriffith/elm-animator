module InternalAnim.Hash exposing (bezierNormalized, float)

{-|

@docs hashFloat

-}

import Bezier
import InternalAnim.Bits as Bits


{-| Will only capture 2 decimal places.
-}
float : Float -> String
float f =
    String.fromInt (round (f * 100))


bezierNormalized : Bezier.Spline -> String
bezierNormalized spline =
    let
        two =
            Bezier.controlOne spline

        three =
            Bezier.controlTwo spline
    in
    "b" ++ String.fromInt (Bits.value (Bits.store4Float two.x two.y three.x three.y))
