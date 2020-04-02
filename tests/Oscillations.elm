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
    Interpolate.withStandardDefault <|
        case event of
            Hufflepuff ->
                Animator.wave 0 1
                    |> Animator.loop (Animator.millis 100)

            Griffyndor ->
                Animator.at 300

            Slytherin ->
                Animator.at 700

            Ravenclaw ->
                Animator.at 1000


oscillators event =
    case event of
        Hufflepuff ->
            Animator.wave 0 1
                |> Animator.loop (Animator.millis 100)

        Griffyndor ->
            Animator.wave 0 1
                |> Animator.loop (Animator.millis 200)

        Slytherin ->
            Animator.wave 0 1
                |> Animator.loop (Animator.millis 300)

        Ravenclaw ->
            Animator.wave 0 1
                |> Animator.loop (Animator.millis 400)


single =
    Animator.init Hufflepuff
        |> Timeline.update (Time.millisToPosix 0)
        |> Timeline.update (Time.millisToPosix 50)


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
        |> Timeline.update (Time.millisToPosix 0)


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
                    Interpolate.Oscillate _ dir fn ->
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
                        Interpolate.details single toPosition
                in
                Expect.within
                    (Absolute 0.001)
                    position
                    1.0
        , test "wave on timeline, second period" <|
            \_ ->
                let
                    { position, velocity } =
                        Interpolate.details (Timeline.update (Time.millisToPosix 150) single) toPosition
                in
                Expect.within
                    (Absolute 0.001)
                    position
                    1.0
        , test "Scheduling doesn't eliminate existing thing" <|
            \_ ->
                let
                    myTimeline =
                        Animator.init Hufflepuff
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.go (Animator.seconds 1) Griffyndor
                            |> Timeline.update (Time.millisToPosix 0)

                    position =
                        Animator.move myTimeline oscillators
                in
                Expect.within
                    (Absolute 0.001)
                    position
                    0
        ]
