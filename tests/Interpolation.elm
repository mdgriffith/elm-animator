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


sinInterpolation x =
    sin (2 * pi * x)


cosInterpolation x =
    cos (2 * pi * x)


easingDerivatives : Test
easingDerivatives =
    describe "Calc Derivative of Easing Fn"
        [ test "deriv of sin(0) == cos(0)" <|
            \_ ->
                let
                    dx =
                        Interpolate.derivativeOfEasing sinInterpolation oneSecond 0
                            |> Pixels.inPixelsPerSecond
                in
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
                            |> Pixels.inPixelsPerSecond
                in
                Expect.within
                    (Absolute 0.001)
                    -- we divide by 2pi here because we're squishing our sin function into an easing 0-1 range.
                    -- when it is normally 0-2pi
                    -- we care because we're using cosine as our checking function
                    (dx / (2 * pi))
                    (cos x)
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
                        1

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
                        1000 * avg first second

                    -- _ =
                    --     Debug.log "deriv" ( time, expected, one.velocity )
                in
                Expect.within
                    (Absolute 1)
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
            Animator.wave 390 410
                |> Animator.oscillate (Animator.millis 200)

        Slytherin ->
            Animator.to 700

        Ravenclaw ->
            Animator.to 1000
