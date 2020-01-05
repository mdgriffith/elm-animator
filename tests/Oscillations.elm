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


type House
    = Hufflepuff
    | Griffyndor
    | Slytherin
    | Ravenclaw


toPosition event =
    case event of
        Hufflepuff ->
            Animator.wave 0 1
                |> Animator.loop (Animator.millis 100)

        Griffyndor ->
            Animator.to 300

        Slytherin ->
            Animator.to 700

        Ravenclaw ->
            Animator.to 1000


single =
    Animator.init Hufflepuff
        |> Animator.update (Time.millisToPosix 0)
        |> Animator.update (Time.millisToPosix 50)


four =
    Animator.init Hufflepuff
        |> Animator.queue
            [ Animator.wait (Animator.seconds 0.5)
            , Animator.event (Animator.seconds 1) Griffyndor
            , Animator.wait (Animator.seconds 1.0)
            , Animator.event (Animator.seconds 1) Slytherin
            , Animator.wait (Animator.seconds 1.0)
            , Animator.event (Animator.seconds 1) Ravenclaw
            , Animator.wait (Animator.seconds 1.0)
            ]
        |> Animator.update (Time.millisToPosix 0)


oscillations =
    describe "oscillations"
        [ test "wave" <|
            \_ ->
                let
                    osc =
                        Animator.wave 0 1
                            |> Animator.loop (Animator.millis 100)
                in
                case osc of
                    Interpolate.Oscillate _ _ dir fn ->
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
        , test "wave on timeline, halfwy" <|
            \_ ->
                let
                    { position, velocity } =
                        Animator.move single toPosition
                in
                Expect.within
                    (Absolute 0.001)
                    position
                    1.0
        , test "wave on timeline, second period" <|
            \_ ->
                let
                    { position, velocity } =
                        Animator.move (Animator.update (Time.millisToPosix 150) single) toPosition
                in
                Expect.within
                    (Absolute 0.001)
                    position
                    1.0
        ]
