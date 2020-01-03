module Scheduling exposing (interruptions, queueing)

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


type Event
    = Starting
    | One
    | Two
    | Three
    | Four
    | Unreachable


timeline =
    Animator.init Starting
        |> Animator.update (Time.millisToPosix 0)


scheduling =
    describe "Interruptions and"
        [ queueing

        -- , interruptions
        ]


qty =
    Quantity.Quantity


occur =
    Timeline.Occurring


toVals event =
    case event of
        Starting ->
            0

        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4

        Unreachable ->
            -1


valueAtEquals time val tl =
    let
        new =
            Animator.update (Time.millisToPosix time) tl
    in
    Expect.within
        (Absolute 0.001)
        (Animator.float new toVals)
        val


queueing =
    let
        newTimeline =
            timeline
                |> Animator.queue
                    [ Animator.wait (Animator.seconds 1.0)
                    , Animator.event (Animator.seconds 1) One
                    , Animator.wait (Animator.seconds 1.0)
                    , Animator.event (Animator.seconds 1) Two
                    , Animator.wait (Animator.seconds 1.0)
                    , Animator.event (Animator.seconds 1) Three
                    , Animator.wait (Animator.seconds 1.0)
                    ]
                |> Animator.update (Time.millisToPosix 0)
    in
    describe "Queueing"
        [ test "Simple queue" <|
            \_ ->
                Expect.equal
                    newTimeline
                    (Timeline.Timeline
                        { events =
                            Timeline.Timetable
                                [ Timeline.Line (qty 0)
                                    (occur Starting (qty 0) (Just (qty 1)))
                                    [ occur One (qty 2000) (Just (qty 1))
                                    , occur Two (qty 4000) (Just (qty 1))
                                    , occur Three (qty 6000) (Just (qty 1))
                                    ]
                                ]
                        , initial = Starting
                        , interruption = []
                        , now = qty 0
                        , queued = Nothing
                        , running = True
                        }
                    )
        , test "Resulting timeline foldp's correctly" <|
            \_ ->
                Expect.all
                    [ valueAtEquals 0 0
                    , valueAtEquals 2000 1
                    , valueAtEquals 4000 2
                    , valueAtEquals 6000 3
                    ]
                    newTimeline
        ]


interruptions =
    let
        newTimeline =
            timeline
                |> Animator.queue
                    [ Animator.wait (Animator.seconds 1.0)
                    , Animator.event (Animator.seconds 1) One
                    , Animator.wait (Animator.seconds 1.0)
                    , Animator.event (Animator.seconds 1) Two
                    , Animator.wait (Animator.seconds 1.0)
                    , Animator.event (Animator.seconds 1) Unreachable
                    , Animator.wait (Animator.seconds 1.0)
                    ]
                |> Animator.update (Time.millisToPosix 0)
                |> Animator.interrupt
                    [ Animator.wait (Animator.seconds 1.0)
                    , Animator.event (Animator.seconds 1) Three
                    , Animator.wait (Animator.seconds 1.0)
                    , Animator.event (Animator.seconds 1) Four
                    ]
                |> Animator.update (Time.millisToPosix 4000)
    in
    describe "Interruptions"
        [ test "Correctly schedules" <|
            \_ ->
                Expect.equal
                    newTimeline
                    (Timeline.Timeline
                        { events =
                            Timeline.Timetable
                                [ Timeline.Line (qty 0)
                                    (occur Starting (qty 0) (Just (qty 1)))
                                    [ occur One (qty 2000) (Just (qty 1))
                                    , occur Two (qty 4000) (Just (qty 1))
                                    , occur Unreachable (qty 6000) (Just (qty 1))
                                    ]
                                , Timeline.Line (qty 4000)
                                    (occur Three (qty 4000) (Just (qty 1)))
                                    [ occur Four (qty 6000) Nothing
                                    ]
                                ]
                        , initial = Starting
                        , interruption = []
                        , now = qty 4000
                        , queued = Nothing
                        , running = True
                        }
                    )
        , test "Correctly folds" <|
            \_ ->
                Expect.all
                    [ valueAtEquals 0 0
                    , valueAtEquals 2000 1
                    , valueAtEquals 4000 2
                    , valueAtEquals 6000 4
                    ]
                    newTimeline
        , fuzz (Fuzz.intRange 0 6000) "Never reaches unreachable event" <|
            \t ->
                let
                    new =
                        Animator.update (Time.millisToPosix t) newTimeline
                in
                Expect.atLeast 0
                    (Animator.float new toVals)
        ]
