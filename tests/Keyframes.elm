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
        [ Test.only <|
            Test.test "between 0 and 1" <|
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
                    Expect.equal "" keyframes
        , Test.test "Wobble between 0 and 1" <|
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
                            transition.wobble
                in
                Expect.equal "" keyframes
        ]
