module Keyframes exposing (suite)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer)
import InternalAnim.Transition as Transition
import Test


transition =
    { standard = Transition.standard
    , linear = Transition.linear
    , wobble =
        Transition.wobble
            { wobble = 1
            , quickness = 0
            }
    }


suite =
    Test.describe "Standard transitions"
        [ Test.test "between 0 and 1" <|
            \_ ->
                let
                    keyframes =
                        Transition.keyframes
                            (\t ->
                                -- lerp t startPos val
                                -- \v -> Debug.toString v ++ "!important"
                                String.fromFloat t
                            )
                            0
                            100
                            transition.standard
                in
                Expect.equal
                    "0% {animation-timing-function:cubic-bezier(0.4,0,0.2,1);}100% {1;}"
                    keyframes
        , Test.test "Wobble between 0 and 1" <|
            \_ ->
                let
                    keyframes =
                        Transition.keyframes
                            (\t ->
                                String.fromFloat t
                            )
                            0
                            100
                            transition.wobble
                in
                Expect.equal "0% {animation-timing-function:cubic-bezier(0.17,0.17,0.7,0.7);}12% {0.6042289461148446;animation-timing-function:cubic-bezier(0.3,0.34,0.7,0.83);}25% {1.153756824025458;animation-timing-function:cubic-bezier(0.3,-13.13,0.7,-3.36);}37% {1.148814684968792;animation-timing-function:cubic-bezier(0.3,0.21,0.7,0.79);}50% {1.0012865341210144;animation-timing-function:cubic-bezier(0.3,0.64,0.7,0.92);}62% {0.9607925658196587;animation-timing-function:cubic-bezier(0.3,0.03,0.7,0.73);}75% {0.9885468559693521;animation-timing-function:cubic-bezier(0.3,0.39,0.7,0.84);}87% {1.0071801184109312;animation-timing-function:cubic-bezier(0.3,-0.86,0.7,0.46);}100% {1005.0803416737684;}" keyframes
        ]
