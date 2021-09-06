module TimelineState exposing
    ( arrived
    , arrivedAt
    , current
    , previous
    , upcoming
    )

{-| -}

import Animator.Timeline
import Duration
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Internal.Estimation as Estimate
import Internal.Timeline as Timeline
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
        Animator.Timeline.init Starting
            |> Timeline.update (Time.millisToPosix 0)
    , double =
        { begin =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                |> Timeline.update (Time.millisToPosix 0)
        , transitioning =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 500)
        , arrived =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 1000)
        , after =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 1001)
        }
    , queued =
        { begin =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.queue
                    [ Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) One
                    , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Two
                    , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Three
                    ]
                |> Timeline.update (Time.millisToPosix 0)
        , transitioningToOne =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.queue
                    [ Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) One
                    , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Two
                    , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Three
                    ]
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 500)
        , transitioningToTwo =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.queue
                    [ Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) One
                    , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Two
                    , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Three
                    ]
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 1500)
        , transitioningToThree =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.queue
                    [ Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) One
                    , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Two
                    , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Three
                    ]
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 2500)
        , atOne =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.queue
                    [ Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) One
                    , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Two
                    , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Three
                    ]
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 1000)
        , atTwo =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.queue
                    [ Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) One
                    , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Two
                    , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Three
                    ]
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 2000)
        , atThree =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.queue
                    [ Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) One
                    , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Two
                    , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Three
                    ]
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 3000)
        , afterThree =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.queue
                    [ Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) One
                    , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Two
                    , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Three
                    ]
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 3001)
        }
    , interruptions =
        { -- we want to chain interruptions together so that they all interrupt each other.
          oneStart =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                |> Timeline.update (Time.millisToPosix 500)
        , oneDuring =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Timeline.update (Time.millisToPosix 1000)
        , oneFinished =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Timeline.update (Time.millisToPosix 1500)
        , oneAfter =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Timeline.update (Time.millisToPosix 1501)
        , twoStart =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) Two
                |> Timeline.update (Time.millisToPosix 1000)
        , twoDuring =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) Two
                |> Timeline.update (Time.millisToPosix 1000)
                |> Timeline.update (Time.millisToPosix 1500)
        , twoFinished =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) Two
                |> Timeline.update (Time.millisToPosix 1000)
                |> Timeline.update (Time.millisToPosix 2000)
        , twoAfter =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) Two
                |> Timeline.update (Time.millisToPosix 1000)
                |> Timeline.update (Time.millisToPosix 2001)
        , threeStart =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) Two
                |> Timeline.update (Time.millisToPosix 1000)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) Three
                |> Timeline.update (Time.millisToPosix 1500)
        , threeDuring =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) Two
                |> Timeline.update (Time.millisToPosix 1000)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) Three
                |> Timeline.update (Time.millisToPosix 1500)
                |> Timeline.update (Time.millisToPosix 2000)
        , threeFinished =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) Two
                |> Timeline.update (Time.millisToPosix 1000)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) Three
                |> Timeline.update (Time.millisToPosix 1500)
                |> Timeline.update (Time.millisToPosix 2500)
        , threeAfter =
            Animator.Timeline.init Starting
                |> Timeline.update (Time.millisToPosix 0)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                |> Timeline.update (Time.millisToPosix 500)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) Two
                |> Timeline.update (Time.millisToPosix 1000)
                |> Animator.Timeline.to (Animator.Timeline.ms 1000) Three
                |> Timeline.update (Time.millisToPosix 1500)
                |> Timeline.update (Time.millisToPosix 2501)
        }
    }


current =
    describe "Current"
        [ test "is current value at resting value" <|
            \_ ->
                let
                    start =
                        Animator.Timeline.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                in
                Expect.equal (Animator.Timeline.current start)
                    Starting
        , test "is current value when arrived" <|
            \_ ->
                let
                    timeline =
                        Animator.Timeline.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 1000)
                in
                Expect.equal (Animator.Timeline.current timeline)
                    One
        , test "when queueing many events, is current value" <|
            \_ ->
                let
                    timeline =
                        Animator.Timeline.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.Timeline.queue
                                [ Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) One
                                , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Two
                                , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Three
                                ]
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 2500)
                in
                Expect.equal (Animator.Timeline.current timeline)
                    Three
        , test "when queueing many events, at event, is current value" <|
            \_ ->
                let
                    timeline =
                        Animator.Timeline.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.Timeline.queue
                                [ Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) One
                                , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Two
                                , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Three
                                ]
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 3000)
                in
                Expect.equal (Animator.Timeline.current timeline)
                    Three
        , test "starting state" <|
            \_ ->
                Expect.equal
                    (Animator.Timeline.current timelines.single)
                    Starting
        , describe "double"
            [ test "begin" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.double.begin)
                        One
            , test "transitioning" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.double.transitioning)
                        One
            , test "arrived" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.double.arrived)
                        One
            , test "after" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.double.after)
                        One
            ]
        , describe "queued"
            -- queued
            [ test "begin" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.queued.begin)
                        One
            , test "transitioningToOne" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.queued.transitioningToOne)
                        One
            , test "transitioningToTwo" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.queued.transitioningToTwo)
                        Two
            , test "transitioningToThree" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.queued.transitioningToThree)
                        Three
            , test "atOne" <|
                \_ ->
                    -- as soon as we arrive at One, we are progressing to Two
                    Expect.equal
                        (Animator.Timeline.current timelines.queued.atOne)
                        Two
            , test "atTwo" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.queued.atTwo)
                        Three
            , test "atThree" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.queued.atThree)
                        Three
            , test "afterThree" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.queued.afterThree)
                        Three
            ]
        , describe "interruptions"
            [ -- One
              test "One start" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.interruptions.oneStart)
                        One
            , test "One during" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.interruptions.oneDuring)
                        One
            , test "One finished" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.interruptions.oneFinished)
                        One
            , test "One after" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.interruptions.oneAfter)
                        One

            -- TWO
            , test "Two start" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.interruptions.twoStart)
                        Two
            , test "Two during" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.interruptions.twoDuring)
                        Two
            , test "Two finished" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.interruptions.twoFinished)
                        Two
            , test "Two after" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.interruptions.twoAfter)
                        Two

            -- THREE
            , test "Three start" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.interruptions.threeStart)
                        Three
            , test "Three during" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.interruptions.threeDuring)
                        Three
            , test "Three finished" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.interruptions.threeFinished)
                        Three
            , test "Three after" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.current timelines.interruptions.threeAfter)
                        Three
            ]
        ]


arrived =
    describe "Arrived"
        [ test "value is at resting value" <|
            \_ ->
                let
                    start =
                        Animator.Timeline.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                in
                Expect.equal (Animator.Timeline.arrived start)
                    Starting
        , test "Arrived value is the previous value when transitioning" <|
            \_ ->
                let
                    timeline =
                        Animator.Timeline.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.Timeline.queue
                                [ Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) One
                                , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Two
                                ]
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 500)
                in
                Expect.equal (Animator.Timeline.arrived timeline)
                    Starting
        , test "Arrived value is the target value when arrived" <|
            \_ ->
                let
                    timeline =
                        Animator.Timeline.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 1000)
                in
                Expect.equal (Animator.Timeline.arrived timeline)
                    One
        , test "at value after an interruption" <|
            \_ ->
                let
                    timeline =
                        Animator.Timeline.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 1500)
                            |> Animator.Timeline.to (Animator.Timeline.ms 1000) Two
                            |> Timeline.update (Time.millisToPosix 2000)
                            |> Animator.Timeline.to (Animator.Timeline.ms 1000) Three
                            |> Timeline.update (Time.millisToPosix 2100)
                            |> Timeline.update (Time.millisToPosix 3100)
                in
                Expect.equal (Animator.Timeline.arrived timeline)
                    Three
        , test "starting state" <|
            \_ ->
                Expect.equal
                    (Animator.Timeline.arrived timelines.single)
                    Starting
        , describe "double"
            [ test "begin" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.double.begin)
                        Starting
            , test "transitioning" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.double.transitioning)
                        Starting
            , test "arrived" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.double.arrived)
                        One
            , test "after" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.double.after)
                        One
            ]
        , describe "queued"
            -- queued
            [ test "begin" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.queued.begin)
                        Starting
            , test "transitioningToOne" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.queued.transitioningToOne)
                        Starting
            , test "transitioningToTwo" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.queued.transitioningToTwo)
                        One
            , test "transitioningToThree" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.queued.transitioningToThree)
                        Two
            , test "atOne" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.queued.atOne)
                        One
            , test "atTwo" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.queued.atTwo)
                        Two
            , test "atThree" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.queued.atThree)
                        Three
            , test "afterThree" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.queued.afterThree)
                        Three
            ]
        , describe "interruptions"
            [ -- One
              test "One start" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.interruptions.oneStart)
                        Starting
            , test "One during" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.interruptions.oneDuring)
                        Starting
            , test "One finished" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.interruptions.oneFinished)
                        One
            , test "One after" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.interruptions.oneAfter)
                        One

            -- TWO
            , test "Two start" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.interruptions.twoStart)
                        Starting
            , test "Two during" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.interruptions.twoDuring)
                        Starting
            , test "Two finished" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.interruptions.twoFinished)
                        Two
            , test "Two after" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.interruptions.twoAfter)
                        Two

            -- THREE
            , test "Three start" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.interruptions.threeStart)
                        Starting
            , test "Three during" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.interruptions.threeDuring)
                        Starting
            , test "Three finished" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.interruptions.threeFinished)
                        Three
            , test "Three after" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.arrived timelines.interruptions.threeAfter)
                        Three
            ]
        ]


previous =
    describe "Previous"
        [ test "at resting value" <|
            \_ ->
                let
                    start =
                        Animator.Timeline.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                in
                Expect.equal (Animator.Timeline.previous start)
                    Starting
        , test "is the previous value when transitioning" <|
            \_ ->
                let
                    timeline =
                        Animator.Timeline.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 500)
                in
                Expect.equal (Animator.Timeline.previous timeline)
                    Starting
        , test "is the previous value when arrived" <|
            \_ ->
                let
                    timeline =
                        Animator.Timeline.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 1000)
                in
                Expect.equal (Animator.Timeline.previous timeline)
                    Starting
        , test "is the previous value after arrived" <|
            \_ ->
                let
                    timeline =
                        Animator.Timeline.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 1500)
                in
                Expect.equal (Animator.Timeline.previous timeline)
                    Starting
        , test "starting state" <|
            \_ ->
                Expect.equal
                    (Animator.Timeline.previous timelines.single)
                    Starting
        , describe "double"
            [ test "begin" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.double.begin)
                        Starting
            , test "transitioning" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.double.transitioning)
                        Starting
            , test "arrived" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.double.arrived)
                        Starting
            , test "after" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.double.after)
                        Starting
            ]
        , describe "queued"
            -- queued
            [ test "begin" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.queued.begin)
                        Starting
            , test "transitioningToOne" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.queued.transitioningToOne)
                        Starting
            , test "transitioningToTwo" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.queued.transitioningToTwo)
                        One
            , test "transitioningToThree" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.queued.transitioningToThree)
                        Two
            , test "atOne" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.queued.atOne)
                        Starting
            , test "atTwo" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.queued.atTwo)
                        One
            , test "atThree" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.queued.atThree)
                        Two
            , test "afterThree" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.queued.afterThree)
                        Two
            ]
        , describe "interruptions"
            [ -- One
              test "One start" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.interruptions.oneStart)
                        Starting
            , test "One during" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.interruptions.oneDuring)
                        Starting
            , test "One finished" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.interruptions.oneFinished)
                        Starting
            , test "One after" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.interruptions.oneAfter)
                        Starting

            -- TWO
            , test "Two start" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.interruptions.twoStart)
                        Starting
            , test "Two during" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.interruptions.twoDuring)
                        Starting
            , test "Two finished" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.interruptions.twoFinished)
                        Starting
            , test "Two after" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.interruptions.twoAfter)
                        Starting

            -- THREE
            , test "Three start" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.interruptions.threeStart)
                        Starting
            , test "Three during" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.interruptions.threeDuring)
                        Starting
            , test "Three finished" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.interruptions.threeFinished)
                        Starting
            , test "Three after" <|
                \_ ->
                    Expect.equal
                        (Animator.Timeline.previous timelines.interruptions.threeAfter)
                        Starting
            ]
        ]


upcoming =
    describe "Upcoming"
        [ test "at resting value" <|
            \_ ->
                let
                    start =
                        Animator.Timeline.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                in
                Expect.equal (Animator.Timeline.upcoming Starting start)
                    False
        , test "is the upcoming value when transitioning" <|
            \_ ->
                let
                    timeline =
                        Animator.Timeline.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 500)
                in
                Expect.equal (Animator.Timeline.upcoming One timeline)
                    True
        , test "is the upcoming value when arrived" <|
            \_ ->
                let
                    timeline =
                        Animator.Timeline.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 1000)
                in
                Expect.equal (Animator.Timeline.upcoming One timeline)
                    False
        , test "is the upcoming value after arrived" <|
            \_ ->
                let
                    timeline =
                        Animator.Timeline.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 1500)
                in
                Expect.equal (Animator.Timeline.upcoming One timeline)
                    False
        , test "Takes effect immediately with queued events" <|
            -- we want this because what if a bunch of events fire in a single animation frame?
            -- we want to be able to only queue once in those cases
            \_ ->
                let
                    timeline =
                        Animator.Timeline.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.Timeline.queue
                                [ Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) One
                                , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Two
                                , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Three
                                ]

                    -- we're not updating yet.
                in
                Expect.equal (Animator.Timeline.upcoming Two timeline)
                    True
        , test "Takes effect immediately with interrupted events" <|
            \_ ->
                let
                    timeline =
                        Animator.Timeline.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.Timeline.interrupt
                                [ Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) One
                                , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Two
                                , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Three
                                ]

                    -- we're not updating yet.
                in
                Expect.equal (Animator.Timeline.upcoming Two timeline)
                    True
        ]


arrivedAt =
    describe "Arrived at"
        [ test "Just arrived at value" <|
            \_ ->
                let
                    timeline =
                        Animator.Timeline.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 500)
                in
                Expect.equal (Animator.Timeline.arrivedAt One (Time.millisToPosix 1001) timeline)
                    True
        , test "Haven't quite arrived" <|
            \_ ->
                let
                    timeline =
                        Animator.Timeline.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.Timeline.to (Animator.Timeline.ms 1000) One
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 500)
                in
                Expect.equal (Animator.Timeline.arrivedAt One (Time.millisToPosix 999) timeline)
                    False
        , test "Arrived after a long queue" <|
            \_ ->
                let
                    timeline =
                        Animator.Timeline.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.Timeline.queue
                                [ Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) One
                                , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Two
                                , Animator.Timeline.transitionTo (Animator.Timeline.ms 1000) Three
                                ]
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 1999)
                in
                Expect.equal (Animator.Timeline.arrivedAt Two (Time.millisToPosix 2001) timeline)
                    True
        ]
