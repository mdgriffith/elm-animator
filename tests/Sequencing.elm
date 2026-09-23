module Sequencing exposing (suite)

{-| Check the sequence representation against independently sampled motion.
The initial state is the position at render time, not at the start of a discarded
transition. An interrupted animation must be sampled before generating its
replacement sequence.
-}

import Expect
import InternalAnim.Move as Move
import InternalAnim.Time as Time
import InternalAnim.Transition as Transition
import InternalAnim.Units as Units
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Movement sequences"
        [ test "A linear sequence reaches its midpoint" <|
            \_ ->
                Move.sequences (Time.millis 0)
                    (Time.millis 1000)
                    (Time.millis 0)
                    (Time.millis 1000)
                    (Move.toWith Transition.linear 1000)
                    []
                    |> expectPosition 500 0 500 (Move.toState 0)
        , test "A standard sequence agrees with direct interpolation" <|
            \_ ->
                let
                    movement =
                        Move.toWith Transition.standard 1000

                    expected =
                        Move.at 0.5 (Time.millis 0) (Time.millis 1000) movement (Move.toState 0)
                            |> .position
                            |> Units.inPixels
                in
                Move.sequences (Time.millis 0) (Time.millis 1000) (Time.millis 0) (Time.millis 1000) movement []
                    |> expectPosition expected 0 500 (Move.toState 0)
        , test "Rendering a linear transition partway through preserves its remaining duration" <|
            \_ ->
                Move.sequences (Time.millis 0)
                    (Time.millis 1000)
                    (Time.millis 250)
                    (Time.millis 1000)
                    (Move.toWith Transition.linear 1000)
                    []
                    |> expectPosition 500 250 500 (Move.toState 250)
        , test "A replacement sequence starts at the interrupted position" <|
            \_ ->
                -- The first motion is 0 -> 1000 over one second. At 500ms it
                -- is at 500; halfway through the replacement 500 -> 2000 it
                -- must be at 1250, not the abandoned destination or 2000.
                Move.sequences (Time.millis 500)
                    (Time.millis 1500)
                    (Time.millis 500)
                    (Time.millis 1500)
                    (Move.toWith Transition.linear 2000)
                    []
                    |> expectPosition 1250 500 1000 (Move.toState 500)
        , test "An upcoming transition retains its delay" <|
            \_ ->
                Move.sequences (Time.millis 500)
                    (Time.millis 1500)
                    (Time.millis 0)
                    (Time.millis 1500)
                    (Move.toWith Transition.linear 1000)
                    []
                    |> expectPosition 250 0 750 (Move.toState 0)
        ]


expectPosition : Float -> Float -> Float -> Move.State -> List (Move.Sequence Float) -> Expect.Expectation
expectPosition expected renderedAt sampledAt initial sequences =
    case sequences of
        [ Move.Sequence 1 delay _ [ Move.Step duration transition target ] ] ->
            let
                start =
                    Time.advanceBy delay (Time.millis renderedAt)

                end =
                    Time.advanceBy duration start

                progress =
                    Time.progress start end (Time.millis sampledAt)
            in
            Transition.atX progress start end transition initial target
                |> .position
                |> Units.inPixels
                -- Bézier inversion is approximate; a quarter pixel across
                -- these 1000px transitions is below visible precision.
                |> Expect.within (Expect.Absolute 0.25) expected

        _ ->
            Expect.fail "Expected one non-repeating, single-step sequence"
