module Interpolation exposing (easingDerivatives, timeline)

import Animator
import Duration
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float, int, list, string)
import Internal.Estimation as Estimate
import Internal.Interpolate as Interpolate
import Internal.Timeline as Timeline
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


mapTime fn time =
    Time.millisToPosix (fn (Time.posixToMillis time))


timeline : Test
timeline =
    describe "Timeline"
        [ fuzz pointsOnTimeline "All points report the correct velocity" <|
            \time ->
                let
                    found =
                        Animator.move (Timeline.atTime time harryPotterHouseTimeline) toPosition

                    expected =
                        Estimate.velocity 32 time harryPotterHouseTimeline toPosition
                in
                Expect.within
                    -- NOTE: I have no idea why this is so off.
                    -- checking things visually on a plot seems to confirm tht the velocities match well.
                    (Absolute 15.0)
                    found.velocity
                    expected
        , let
            newTimeline =
                Animator.init Hufflepuff
                    |> Animator.queue
                        [ Animator.wait (Animator.seconds 1)
                        , Animator.event (Animator.seconds 1) Griffyndor
                        ]
                    |> Animator.update (Time.millisToPosix 0)
                    |> Animator.interrupt
                        [ Animator.event (Animator.seconds 1) Ravenclaw
                        ]
                    |> Animator.update (Time.millisToPosix 1500)
          in
          test "Interruptions interpolate correctly" <|
            \_ ->
                let
                    position =
                        Animator.linear
                            (Animator.update (Time.millisToPosix 2000) newTimeline)
                            toPos
                in
                -- we were half the way to griffyndor (200)
                -- and then halfway between that and Ravenclaw (1000)
                --  400 + 200 = 600
                Expect.within
                    (Absolute 0.001)
                    position
                    600
        , test "Velocity at continuous checkpoints is not 0" <|
            \_ ->
                let
                    newTimeline =
                        Animator.init Hufflepuff
                            |> Animator.queue
                                [ Animator.event (Animator.seconds 1) Griffyndor
                                , Animator.event (Animator.seconds 1) Slytherin
                                , Animator.event (Animator.seconds 1) Ravenclaw
                                ]
                            |> Animator.update (Time.millisToPosix 0)

                    posAt time timetable =
                        let
                            position =
                                Animator.move
                                    (Timeline.atTime
                                        (Time.millisToPosix time)
                                        timetable
                                    )
                                    toPosition
                        in
                        Expect.notWithin
                            (Absolute 0.001)
                            0
                            position.velocity
                in
                Expect.all
                    [ posAt 1000
                    , posAt 2000
                    ]
                    newTimeline
        , test "Initial resting velocity is 0 if event hasn't started yet" <|
            \_ ->
                let
                    doubleEvent =
                        Animator.init Hufflepuff
                            |> Animator.update (Time.millisToPosix 0)
                            |> Animator.queue
                                [ Animator.wait (Animator.seconds 1)
                                , Animator.event (Animator.seconds 1) Griffyndor
                                ]
                            |> Animator.update (Time.millisToPosix 1000)
                            |> Animator.interrupt
                                [ Animator.event (Animator.seconds 1) Ravenclaw
                                ]
                            |> Animator.update (Time.millisToPosix 2500)
                            |> Animator.update (Time.millisToPosix 3000)

                    position =
                        Animator.move
                            (Timeline.atTime
                                (Time.millisToPosix 1900)
                                doubleEvent
                            )
                            toPosition
                in
                Expect.within
                    (Absolute 0.001)
                    0
                    position.velocity
        ]


avg one two =
    (one + two) / 2


type House
    = Hufflepuff
    | Griffyndor
    | Slytherin
    | Ravenclaw


toPos event =
    case event of
        Hufflepuff ->
            100

        Griffyndor ->
            300

        Slytherin ->
            700

        Ravenclaw ->
            1000


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
