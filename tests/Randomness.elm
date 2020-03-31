module Randomness exposing (suite)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float, int, list, string)
import Internal.Random
import Test exposing (..)


suite =
    describe "Sin based random curve"
        [ fuzz Fuzz.float "between 0 and 1" <|
            \seed ->
                Internal.Random.random seed 0 1
                    |> Expect.all
                        [ Expect.atMost 1
                        , Expect.atLeast 0
                        ]
        ]
