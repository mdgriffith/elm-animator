module Oscillations exposing (oscillations)

import Animator
import Duration
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float, int, list, string)
import Internal.Interpolate as Interpolate
import Internal.Timeline as Timeline
import Pixels
import Quantity
import Test exposing (..)
import Time


oscillations =
    describe "oscillations"
        [ test "wave" <|
            \_ ->
                let
                    osc =
                        Animator.wave 0 1
                            |> Animator.oscillate (Animator.millis 100)
                in
                case osc of
                    Interpolate.Oscillate dir fn ->
                        Expect.all
                            [ \f ->
                                Expect.within (Absolute 0.01)
                                    (f 0)
                                    0
                            , \f ->
                                Expect.within (Absolute 0.001)
                                    (f 0.5)
                                    1
                            , \f ->
                                Expect.within (Absolute 0.01)
                                    (f 1)
                                    0
                            ]
                            fn

                    _ ->
                        Expect.fail "Creating an oscillator did not create an oscillator"
        ]
