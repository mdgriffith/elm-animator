module Scheduling exposing (cleaning, interruptions, queueing)

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
    | Five
    | Unreachable


timeline =
    Animator.init Starting
        |> Animator.update (Time.millisToPosix 0)


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

        Five ->
            5

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
        [ describe "Simple"
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
        , test "Queue after some time" <|
            \_ ->
                let
                    queued =
                        timeline
                            |> Animator.update (Time.millisToPosix 3000)
                            |> Animator.queue
                                [ Animator.event (Animator.seconds 1) One
                                ]
                            |> Animator.update (Time.millisToPosix 4000)
                in
                -- specifically we expect the dwell time on `Starting` to be extended
                Expect.equal
                    queued
                    (Timeline.Timeline
                        { events =
                            Timeline.Timetable
                                [ Timeline.Line (qty 0)
                                    (occur Starting (qty 0) (Just (qty 4)))
                                    [ occur One (qty 5000) Nothing
                                    ]
                                ]
                        , initial = Starting
                        , interruption = []
                        , now = qty 4000
                        , queued = Nothing
                        , running = True
                        }
                    )
        , test "Queue twice after some time" <|
            \_ ->
                let
                    queued =
                        timeline
                            |> Animator.update (Time.millisToPosix 3000)
                            |> Animator.queue
                                [ Animator.event (Animator.seconds 1) One
                                ]
                            |> Animator.update (Time.millisToPosix 4000)
                            |> Animator.queue
                                [ Animator.event (Animator.seconds 1) Two
                                ]
                            |> Animator.update (Time.millisToPosix 7000)
                in
                -- specifically we expect the dwell time on `Starting` and `One` to be extended
                Expect.equal
                    queued
                    (Timeline.Timeline
                        { events =
                            Timeline.Timetable
                                [ Timeline.Line (qty 0)
                                    (occur Starting (qty 0) (Just (qty 4)))
                                    [ occur One (qty 5000) (Just (qty 2))
                                    , occur Two (qty 8000) Nothing
                                    ]
                                ]
                        , initial = Starting
                        , interruption = []
                        , now = qty 7000
                        , queued = Nothing
                        , running = True
                        }
                    )
        , test "Adding an initial wait is equivalent to simply updating later" <|
            \_ ->
                Expect.equal
                    (timeline
                        |> Animator.queue
                            [ Animator.wait (Animator.seconds 2)
                            , Animator.event (Animator.seconds 1) One
                            ]
                        |> Animator.update (Time.millisToPosix 0)
                        -- move the clock forward to 2s so that `now` matches
                        |> Animator.update (Time.millisToPosix 2000)
                    )
                    (timeline
                        |> Animator.queue
                            [ Animator.event (Animator.seconds 1) One
                            ]
                        |> Animator.update (Time.millisToPosix 2000)
                    )
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
                |> Animator.update (Time.millisToPosix 3000)
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

                                -- we scheduled at 3000
                                -- but there's a wait for 1000
                                -- so the new line actually starts at 4000.
                                , Timeline.Line (qty 4000)
                                    -- then take 1 second to transition to Three
                                    (occur Three (qty 5000) (Just (qty 1)))
                                    -- we then wait a second
                                    -- then take a second to transition to Four
                                    [ occur Four (qty 7000) Nothing
                                    ]
                                ]
                        , initial = Starting
                        , interruption = []
                        , now = qty 3000
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
                    , valueAtEquals 5000 3
                    , valueAtEquals 7000 4
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
        , test "Correctly schedules when an interruption already occurred" <|
            \_ ->
                let
                    doubleInterrupted =
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
                            |> Animator.update (Time.millisToPosix 3000)
                            |> Animator.interrupt
                                [ Animator.event (Animator.seconds 1) Two
                                , Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) One
                                ]
                            |> Animator.update (Time.millisToPosix 4500)
                in
                Expect.equal
                    doubleInterrupted
                    (Timeline.Timeline
                        { events =
                            Timeline.Timetable
                                [ Timeline.Line (qty 0)
                                    (occur Starting (qty 0) (Just (qty 1)))
                                    [ occur One (qty 2000) (Just (qty 1))
                                    , occur Two (qty 4000) (Just (qty 1))
                                    , occur Unreachable (qty 6000) (Just (qty 1))
                                    ]

                                -- we scheduled at 3000
                                -- but there's a wait for 1000
                                -- so the new line actually starts at 4000.
                                , Timeline.Line (qty 4000)
                                    -- then take 1 second to transition to Three
                                    (occur Three (qty 5000) (Just (qty 1)))
                                    -- we then wait a second
                                    -- then take a second to transition to Four
                                    [ occur Four (qty 7000) Nothing
                                    ]
                                , Timeline.Line (qty 4500)
                                    (occur Two (qty 5500) (Just (qty 1)))
                                    [ occur One (qty 7500) Nothing
                                    ]
                                ]
                        , initial = Starting
                        , interruption = []
                        , now = qty 4500
                        , queued = Nothing
                        , running = True
                        }
                    )
        , test "Queue timeline instead of interrupt if given after the existing timeline" <|
            \_ ->
                let
                    baseline =
                        timeline
                            |> Animator.queue
                                [ Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) One
                                , Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) Two
                                , Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) Three
                                ]
                            |> Animator.update (Time.millisToPosix 0)

                    interruptedAfterFinish =
                        baseline
                            |> Animator.interrupt
                                [ Animator.event (Animator.seconds 1) Four
                                , Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) Five
                                ]
                            |> Animator.update (Time.millisToPosix 6000)
                in
                Expect.equal
                    interruptedAfterFinish
                    (Timeline.Timeline
                        { events =
                            Timeline.Timetable
                                [ Timeline.Line (qty 0)
                                    (occur Starting (qty 0) (Just (qty 1)))
                                    [ occur One (qty 2000) (Just (qty 1))
                                    , occur Two (qty 4000) (Just (qty 1))
                                    , occur Three (qty 6000) (Just (qty 0))
                                    , occur Four (qty 7000) (Just (qty 1))
                                    , occur Five (qty 9000) Nothing
                                    ]
                                ]
                        , initial = Starting
                        , interruption = []
                        , now = qty 6000
                        , queued = Nothing
                        , running = True
                        }
                    )
        , test "Queue timeline instead if given long after existing" <|
            \_ ->
                let
                    baseline =
                        timeline
                            |> Animator.queue
                                [ Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) One
                                , Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) Two
                                , Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) Three
                                ]
                            |> Animator.update (Time.millisToPosix 0)

                    interruptedAfterFinish =
                        baseline
                            |> Animator.interrupt
                                [ Animator.event (Animator.seconds 1) Four
                                , Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) Five
                                ]
                            |> Animator.update (Time.millisToPosix 8000)
                in
                Expect.equal
                    interruptedAfterFinish
                    (Timeline.Timeline
                        { events =
                            Timeline.Timetable
                                [ Timeline.Line (qty 0)
                                    (occur Starting (qty 0) (Just (qty 1)))
                                    [ occur One (qty 2000) (Just (qty 1))
                                    , occur Two (qty 4000) (Just (qty 1))
                                    , occur Three (qty 6000) (Just (qty 2))
                                    , occur Four (qty 9000) (Just (qty 1))
                                    , occur Five (qty 11000) Nothing
                                    ]
                                ]
                        , initial = Starting
                        , interruption = []
                        , now = qty 8000
                        , queued = Nothing
                        , running = True
                        }
                    )
        ]


cleaning =
    describe "Cleaning"
        [ test "Marked as running correctly" <|
            \_ ->
                let
                    lines =
                        [ Timeline.Line
                            (qty 1578168889621)
                            (occur False (qty 1578168889621) Nothing)
                            [ occur True (qty 1578168895231) Nothing
                            ]
                        , Timeline.Line
                            -- same as now
                            (qty 1578168893838)
                            -- 1000ms later
                            (occur False (qty 1578168895838) Nothing)
                            []
                        ]

                    now =
                        qty 1578168893838
                in
                Expect.true "This timeline at this time should still be active"
                    (Timeline.linesAreActive now lines)
        , test "Marked as running correctly, now after interuption" <|
            \_ ->
                let
                    lines =
                        [ Timeline.Line
                            (qty 1578168889621)
                            (occur False (qty 1578168889621) Nothing)
                            [ occur True (qty 1578168895231) Nothing
                            ]
                        , Timeline.Line
                            -- same as now
                            (qty 1578168893838)
                            -- 1000ms later
                            (occur False (qty 1578168895838) Nothing)
                            []
                        ]

                    now =
                        qty 1578168893839
                in
                Expect.true "This timeline at this time should still be active"
                    (Timeline.linesAreActive now lines)
        ]
