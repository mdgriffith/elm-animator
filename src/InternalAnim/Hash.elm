module InternalAnim.Hash exposing (bezier, float)

{-|

@docs hashFloat

-}

import Bezier
import InternalAnim.Bits as Bits


float : Float -> String
float f =
    let
        base =
            floor f

        decimal =
            floor (100 * (f - toFloat base))
    in
    String.fromInt base ++ "_" ++ String.fromInt decimal


dash : String
dash =
    "-"


bezier : Bezier.Spline -> String
bezier spline =
    let
        one =
            Bezier.first spline

        two =
            Bezier.controlOne spline

        three =
            Bezier.controlTwo spline

        four =
            Bezier.last spline
    in
    String.fromInt (Bits.value (Bits.store4Float one.x one.y two.x two.y))
        ++ dash
        ++ String.fromInt (Bits.value (Bits.store4Float three.x three.y four.x four.y))
