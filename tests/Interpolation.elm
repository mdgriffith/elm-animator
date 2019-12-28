module Interpolation exposing (easingDerivatives, timeline)

import Animator
import Duration
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float, int, list, string)
import Internal.Interpolate as Interpolate
import Pixels
import Quantity
import Test exposing (..)
import Time


oneSecond =
    Duration.seconds 1


halfSecond =
    Duration.seconds 0.5


sinInterpolation x =
    sin (turns x)


cosInterpolation x =
    cos (turns x)


easingDerivatives : Test
easingDerivatives =
    describe "Calc Derivative of Easing Fn"
        [ test "deriv of a linear fn is constant" <|
            \_ ->
                let
                    dx =
                        Interpolate.derivativeOfEasing identity oneSecond 0.5
                            |> Pixels.inPixelsPerSecond
                in
                Expect.within (Absolute 0.01) dx 1

        {- Soooooo, the derivative of sin is cos. great

           But we also want to use `sin` as our easing function.

           So, the domain is 0-1 instead of 0-2pi

           That means we both need to feed `turns` to `cos`, but also scale our output by `2 * pi`

           (think about it, if we squish a sin function, then the slopes get steeper.)

        -}
        , test "deriv of sin(0) == cos(0)" <|
            \_ ->
                let
                    dx =
                        Interpolate.derivativeOfEasing sinInterpolation oneSecond 0
                            |> Pixels.inPixelsPerSecond
                in
                Expect.within
                    (Absolute 0.1)
                    dx
                    (cos (turns 0) * 2 * pi)
        , test "deriv of sin(1) == cos(1)" <|
            \_ ->
                let
                    dx =
                        Interpolate.derivativeOfEasing sinInterpolation oneSecond 1
                            |> Pixels.inPixelsPerSecond
                in
                Expect.within
                    (Absolute 0.1)
                    dx
                    (cos (turns 1) * 2 * pi)
        , fuzz Fuzz.percentage "fuzz deriv of sin(0) == cos(0)" <|
            \x ->
                let
                    dx =
                        Interpolate.derivativeOfEasing sinInterpolation oneSecond x
                            |> Pixels.inPixelsPerSecond
                in
                Expect.within
                    (Absolute 0.01)
                    (dx / (2 * pi))
                    (cos (turns x))
        ]


pointsOnTimeline =
    Fuzz.map Time.millisToPosix
        (Fuzz.intRange -160 6500)


harryPotterHouseTimeline =
    Animator.init (Time.millisToPosix 0) Hufflepuff
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


mapTime fn time =
    Time.millisToPosix (fn (Time.posixToMillis time))


timeline : Test
timeline =
    describe "Timeline"
        [ fuzz pointsOnTimeline "All points report the correct velocity" <|
            \time ->
                let
                    resolution =
                        8

                    before =
                        mapTime (\t -> t - resolution) time

                    after =
                        mapTime (\t -> t + resolution) time

                    zero =
                        Animator.move (Animator.update before harryPotterHouseTimeline) toPosition

                    one =
                        Animator.move (Animator.update time harryPotterHouseTimeline) toPosition

                    two =
                        Animator.move (Animator.update after harryPotterHouseTimeline) toPosition

                    first =
                        (one.position - zero.position) / resolution

                    second =
                        (two.position - one.position) / resolution

                    expected =
                        -- 1000 * avg first second
                        1000 * (two.position - zero.position) / (2 * resolution)
                in
                Expect.within
                    -- my guess is that this is such a wide margin
                    -- because the method for calculating velocity in this function could be better.
                    (Absolute 40)
                    one.velocity
                    expected
        ]


avg one two =
    (one + two) / 2


type House
    = Hufflepuff
    | Griffyndor
    | Slytherin
    | Ravenclaw


toPosition event =
    case event of
        Hufflepuff ->
            Animator.to 100

        Griffyndor ->
            Animator.to 300

        Slytherin ->
            Animator.to 700

        Ravenclaw ->
            Animator.to 1000
