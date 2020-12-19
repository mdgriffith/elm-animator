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



{- TIMELINES -}


timelines =
    { single =
        Animator.init Starting
            |> Timeline.update (Time.millisToPosix 0)
    , double =
        { begin =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.go (Animator.seconds 1) One
                |> Timeline.update (Time.millisToPosix 0)
        , transitioning =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.go (Animator.seconds 1) One
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 500)
        , arrived =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.go (Animator.seconds 1) One
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 1000)
        , after =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.go (Animator.seconds 1) One
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 1001)
        }
    , queued =
        { begin =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.queue
                    [ Animator.event (Animator.seconds 1) One
                    , Animator.event (Animator.seconds 1) Two
                    , Animator.event (Animator.seconds 1) Three
                    ]
                |> Timeline.update (Time.millisToPosix 0)
        , transitioningToOne =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.queue
                    [ Animator.event (Animator.seconds 1) One
                    , Animator.event (Animator.seconds 1) Two
                    , Animator.event (Animator.seconds 1) Three
                    ]
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 500)
        , transitioningToTwo =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.queue
                    [ Animator.event (Animator.seconds 1) One
                    , Animator.event (Animator.seconds 1) Two
                    , Animator.event (Animator.seconds 1) Three
                    ]
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 1500)
        , transitioningToThree =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.queue
                    [ Animator.event (Animator.seconds 1) One
                    , Animator.event (Animator.seconds 1) Two
                    , Animator.event (Animator.seconds 1) Three
                    ]
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 2500)
        , atOne =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.queue
                    [ Animator.event (Animator.seconds 1) One
                    , Animator.event (Animator.seconds 1) Two
                    , Animator.event (Animator.seconds 1) Three
                    ]
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 1000)
        , atTwo =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.queue
                    [ Animator.event (Animator.seconds 1) One
                    , Animator.event (Animator.seconds 1) Two
                    , Animator.event (Animator.seconds 1) Three
                    ]
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 2000)
        , atThree =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.queue
                    [ Animator.event (Animator.seconds 1) One
                    , Animator.event (Animator.seconds 1) Two
                    , Animator.event (Animator.seconds 1) Three
                    ]
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 3000)
        , afterThree =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.queue
                    [ Animator.event (Animator.seconds 1) One
                    , Animator.event (Animator.seconds 1) Two
                    , Animator.event (Animator.seconds 1) Three
                    ]
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 3001)
        }
    , interruptions =
        { -- we want to chain interruptions together so that they all interrupt each other.
          oneStart =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.go (Animator.seconds 1000) One
                |> Timeline.update (Time.millisToPosix 500)
        , oneDuring =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.go (Animator.seconds 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Timeline.update (Time.millisToPosix 1000)
        , oneFinished =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.go (Animator.seconds 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Timeline.update (Time.millisToPosix 1500)
        , oneAfter =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.go (Animator.seconds 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Timeline.update (Time.millisToPosix 1501)
        , twoStart =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.go (Animator.seconds 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Animator.go (Animator.seconds 1000) Two
                |> Timeline.update (Time.millisToPosix 1000)
        , twoDuring =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.go (Animator.seconds 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Animator.go (Animator.seconds 1000) Two
                |> Timeline.update (Time.millisToPosix 1000)
                |> Timeline.update (Time.millisToPosix 1500)
        , twoFinished =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.go (Animator.seconds 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Animator.go (Animator.seconds 1000) Two
                |> Timeline.update (Time.millisToPosix 1000)
                |> Timeline.update (Time.millisToPosix 2000)
        , twoAfter =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.go (Animator.seconds 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Animator.go (Animator.seconds 1000) Two
                |> Timeline.update (Time.millisToPosix 1000)
                |> Timeline.update (Time.millisToPosix 2001)
        , threeStart =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.go (Animator.seconds 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Animator.go (Animator.seconds 1000) Two
                |> Timeline.update (Time.millisToPosix 1000)
                |> Animator.go (Animator.seconds 1000) Three
                |> Timeline.update (Time.millisToPosix 1500)
        , threeDuring =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.go (Animator.seconds 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Animator.go (Animator.seconds 1000) Two
                |> Timeline.update (Time.millisToPosix 1000)
                |> Animator.go (Animator.seconds 1000) Three
                |> Timeline.update (Time.millisToPosix 1500)
                |> Timeline.update (Time.millisToPosix 2000)
        , threeFinished =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.go (Animator.seconds 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Animator.go (Animator.seconds 1000) Two
                |> Timeline.update (Time.millisToPosix 1000)
                |> Animator.go (Animator.seconds 1000) Three
                |> Timeline.update (Time.millisToPosix 1500)
                |> Timeline.update (Time.millisToPosix 2500)
        , threeAfter =
            Animator.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.go (Animator.seconds 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Animator.go (Animator.seconds 1000) Two
                |> Timeline.update (Time.millisToPosix 1000)
                |> Animator.go (Animator.seconds 1000) Three
                |> Timeline.update (Time.millisToPosix 1500)
                |> Timeline.update (Time.millisToPosix 2501)
        }
    }


current =
    only <|
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
            , test "starting state" <|
                \_ ->
                    Expect.equal
                        (Animator.current timelines.single)
                        Starting
            , describe "double"
                [ test "begin" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.double.begin)
                            One
                , test "transitioning" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.double.transitioning)
                            One
                , test "arrived" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.double.transitioning)
                            One
                , test "after" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.double.transitioning)
                            One
                ]
            , describe "queued"
                -- queued
                [ test "begin" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.queued.begin)
                            One
                , test "transitioningToOne" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.queued.transitioningToOne)
                            One
                , test "transitioningToTwo" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.queued.transitioningToTwo)
                            Two
                , test "transitioningToThree" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.queued.transitioningToThree)
                            Three
                , test "atOne" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.queued.atOne)
                            One
                , test "atTwo" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.queued.atTwo)
                            Two
                , test "atThree" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.queued.atThree)
                            Three
                , test "afterThree" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.queued.afterThree)
                            Three
                ]
            , describe "interruptions"
                [ -- One
                  test "One start" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.interruptions.oneStart)
                            One
                , test "One during" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.interruptions.oneDuring)
                            One
                , test "One finished" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.interruptions.oneFinished)
                            One
                , test "One after" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.interruptions.oneAfter)
                            One

                -- TWO
                , test "Two start" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.interruptions.twoStart)
                            Two
                , test "Two during" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.interruptions.twoDuring)
                            Two
                , test "Two finished" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.interruptions.twoFinished)
                            Two
                , test "Two after" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.interruptions.twoAfter)
                            Two

                -- THREE
                , test "Three start" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.interruptions.threeStart)
                            Three
                , test "Three during" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.interruptions.threeDuring)
                            Three
                , test "Three finished" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.interruptions.threeFinished)
                            Three
                , test "Three after" <|
                    \_ ->
                        Expect.equal
                            (Animator.current timelines.interruptions.threeAfter)
                            Three
                ]
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
        , test "at value after an interruption" <|
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
                            |> Timeline.update (Time.millisToPosix 3100)
                in
                Expect.equal (Animator.arrived timeline)
                    Three
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
