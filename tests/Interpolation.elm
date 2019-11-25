module Interpolation exposing (..)

import Duration
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float, int, list, string)
import Internal.Interpolate as Interpolate
import Test exposing (..)


oneSecond =
    Duration.seconds 1


sinInterpolation x =
    sin (2 * pi * x)


cosInterpolation x =
    cos (2 * pi * x)


suite : Test
suite =
    describe "Calc Derivative of Easing Fn"
        [ test "deriv of sin(0) == cos(0)" <|
            \_ ->
                let
                    dx =
                        Interpolate.derivativeOfEasing sinInterpolation oneSecond 0
                in
                -- Expect.equal dx
                --     (cos 0)
                Expect.within
                    (Absolute 0.01)
                    -- we divide by 2pi here because we're squishing our sin function into an easing 0-1 range.
                    -- when it is normally 0-2pi
                    -- we care because we're using cosine as our checking function
                    (dx / (2 * pi))
                    (cos 0)
        , fuzz Fuzz.percentage "fuzz deriv of sin(0) == cos(0)" <|
            \x ->
                let
                    dx =
                        Interpolate.derivativeOfEasing sinInterpolation oneSecond x
                in
                Expect.within
                    (Absolute 0.001)
                    -- we divide by 2pi here because we're squishing our sin function into an easing 0-1 range.
                    -- when it is normally 0-2pi
                    -- we care because we're using cosine as our checking function
                    (dx / (2 * pi))
                    (cos x)
        ]
