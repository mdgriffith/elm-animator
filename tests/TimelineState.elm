module TimelineState exposing
    ( arrived
    , arrivedAt
    , current
    , previous
    , upcoming
    )

{-| -}

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


type State
    = Starting
    | One
    | Two
    | Three


current =
    describe "Current"
        [ test "is current value at resting value" <|
            \_ ->
                let
                    start =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                in
                Expect.equal (Animator.current start)
                    Starting
        , test "is current value when arrived" <|
            \_ ->
                let
                    timeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.go (Animator.seconds 1) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 1000)
                in
                Expect.equal (Animator.current timeline)
                    One
        , test "when queueing many events, is current value" <|
            \_ ->
                let
                    timeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.queue
                                [ Animator.event (Animator.seconds 1) One
                                , Animator.event (Animator.seconds 1) Two
                                , Animator.event (Animator.seconds 1) Three
                                ]
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 2500)
                in
                Expect.equal (Animator.current timeline)
                    Three
        , test "when queueing many events, at event, is current value" <|
            \_ ->
                let
                    timeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.queue
                                [ Animator.event (Animator.seconds 1) One
                                , Animator.event (Animator.seconds 1) Two
                                , Animator.event (Animator.seconds 1) Three
                                ]
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 3000)
                in
                Expect.equal (Animator.current timeline)
                    Three
        ]


arrived =
    describe "Arrived"
        [ test "value is at resting value" <|
            \_ ->
                let
                    start =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                in
                Expect.equal (Animator.arrived start)
                    Starting
        , test "Arrived value is the previous value when transitioning" <|
            \_ ->
                let
                    timeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.queue
                                [ Animator.event (Animator.seconds 1) One
                                , Animator.event (Animator.seconds 1) Two
                                ]
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 500)
                in
                Expect.equal (Animator.arrived timeline)
                    Starting
        , test "Arrived value is the target value when arrived" <|
            \_ ->
                let
                    timeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.go (Animator.seconds 1) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 1000)
                in
                Expect.equal (Animator.arrived timeline)
                    One
        ]


previous =
    describe "Previous"
        [ test "at resting value" <|
            \_ ->
                let
                    start =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                in
                Expect.equal (Animator.previous start)
                    Starting
        , test "is the previous value when transitioning" <|
            \_ ->
                let
                    timeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.go (Animator.seconds 1) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 500)
                in
                Expect.equal (Animator.previous timeline)
                    Starting
        , test "is the previous value when arrived" <|
            \_ ->
                let
                    timeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.go (Animator.seconds 1) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 1000)
                in
                Expect.equal (Animator.previous timeline)
                    Starting
        , test "is the previous value after arrived" <|
            \_ ->
                let
                    timeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.go (Animator.seconds 1) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 1500)
                in
                Expect.equal (Animator.previous timeline)
                    Starting
        , test "is the previous value after progressing to second state" <|
            \_ ->
                let
                    timeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.go (Animator.seconds 1) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 1500)
                            |> Animator.go (Animator.seconds 1) Two
                            |> Timeline.update (Time.millisToPosix 2000)
                            |> Animator.go (Animator.seconds 1) Three
                            |> Timeline.update (Time.millisToPosix 2100)
                            |> Timeline.update (Time.millisToPosix 2200)
                in
                Expect.equal (Animator.previous timeline)
                    Two
        ]


upcoming =
    describe "Upcoming"
        [ test "at resting value" <|
            \_ ->
                let
                    start =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                in
                Expect.equal (Animator.upcoming Starting start)
                    False
        , test "is the upcoming value when transitioning" <|
            \_ ->
                let
                    timeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.go (Animator.seconds 1) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 500)
                in
                Expect.equal (Animator.upcoming One timeline)
                    True
        , test "is the upcoming value when arrived" <|
            \_ ->
                let
                    timeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.go (Animator.seconds 1) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 1000)
                in
                Expect.equal (Animator.upcoming One timeline)
                    False
        , test "is the upcoming value after arrived" <|
            \_ ->
                let
                    timeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.go (Animator.seconds 1) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 1500)
                in
                Expect.equal (Animator.upcoming One timeline)
                    False
        , test "Takes effect immediately with queued events" <|
            -- we want this because what if a bunch of events fire in a single animation frame?
            -- we want to be able to only queue once in those cases
            \_ ->
                let
                    timeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.queue
                                [ Animator.event (Animator.seconds 1) One
                                , Animator.event (Animator.seconds 1) Two
                                , Animator.event (Animator.seconds 1) Three
                                ]

                    -- we're not updating yet.
                in
                Expect.equal (Animator.upcoming Two timeline)
                    True
        , test "Takes effect immediately with interrupted events" <|
            \_ ->
                let
                    timeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.interrupt
                                [ Animator.event (Animator.seconds 1) One
                                , Animator.event (Animator.seconds 1) Two
                                , Animator.event (Animator.seconds 1) Three
                                ]

                    -- we're not updating yet.
                in
                Expect.equal (Animator.upcoming Two timeline)
                    True
        ]


arrivedAt =
    describe "Arrived at"
        [ test "Just arrived at value" <|
            \_ ->
                let
                    timeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.go (Animator.seconds 1) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 500)
                in
                Expect.equal (Animator.arrivedAt One (Time.millisToPosix 1001) timeline)
                    True
        , test "Haven't quite arrived" <|
            \_ ->
                let
                    timeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.go (Animator.seconds 1) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 500)
                in
                Expect.equal (Animator.arrivedAt One (Time.millisToPosix 999) timeline)
                    False
        , test "Arrived after a long queue" <|
            \_ ->
                let
                    timeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.queue
                                [ Animator.event (Animator.seconds 1) One
                                , Animator.event (Animator.seconds 1) Two
                                , Animator.event (Animator.seconds 1) Three
                                ]
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 1999)
                in
                Expect.equal (Animator.arrivedAt Two (Time.millisToPosix 2001) timeline)
                    True
        ]
