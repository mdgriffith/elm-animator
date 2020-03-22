module Scheduling exposing (cleaning, interruptions, ordering, queueing, tailRecursion)

import Animator
import Duration
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float, int, list, string)
import Fuzz.Timeline
import Internal.Interpolate as Interpolate
import Internal.Time as Time
import Internal.Timeline as Timeline
import Pixels
import Quantity
import Random
import Result
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
        |> Timeline.update (Time.millisToPosix 0)


qty =
    Quantity.Quantity


occur =
    Timeline.Occurring


toVals event =
    case event of
        Starting ->
            Animator.at 0

        One ->
            Animator.at 1

        Two ->
            Animator.at 2

        Three ->
            Animator.at 3

        Four ->
            Animator.at 4

        Five ->
            Animator.at 5

        Unreachable ->
            Animator.at -1


valueAtEquals time val tl =
    let
        new =
            -- we can't call update because updates have to be monotonic
            Timeline.atTime (Time.millisToPosix time) tl
    in
    Expect.within
        (Absolute 0.001)
        (Animator.move new toVals)
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
                |> Timeline.update (Time.millisToPosix 0)
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
                                        (occur Starting (qty 0) (qty 1000))
                                        [ occur One (qty 2000) (qty 3000)
                                        , occur Two (qty 4000) (qty 5000)
                                        , occur Three (qty 6000) (qty 7000)
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
                            |> Timeline.update (Time.millisToPosix 3000)
                            |> Animator.queue
                                [ Animator.event (Animator.seconds 1) One
                                ]
                            |> Timeline.update (Time.millisToPosix 4000)
                in
                -- specifically we expect the dwell time on `Starting` to be extended
                Expect.equal
                    queued
                    (Timeline.Timeline
                        { events =
                            Timeline.Timetable
                                [ Timeline.Line (qty 0)
                                    (occur Starting (qty 0) (qty 4000))
                                    [ occur One (qty 5000) (qty 5000)
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
                            |> Timeline.update (Time.millisToPosix 3000)
                            |> Animator.queue
                                [ Animator.event (Animator.seconds 1) One
                                ]
                            |> Timeline.updateNoGC (Time.millisToPosix 4000)
                            |> Animator.queue
                                [ Animator.event (Animator.seconds 1) Two
                                ]
                            |> Timeline.updateNoGC (Time.millisToPosix 7000)
                in
                -- specifically we expect the dwell time on `Starting` and `One` to be extended
                Expect.equal
                    queued
                    (Timeline.Timeline
                        { events =
                            Timeline.Timetable
                                [ Timeline.Line (qty 0)
                                    (occur Starting (qty 0) (qty 4000))
                                    [ occur One (qty 5000) (qty 7000)
                                    , occur Two (qty 8000) (qty 8000)
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
                        |> Timeline.update (Time.millisToPosix 0)
                        -- move the clock forward to 2s so that `now` matches
                        |> Timeline.update (Time.millisToPosix 2000)
                    )
                    (timeline
                        |> Animator.queue
                            [ Animator.event (Animator.seconds 1) One
                            ]
                        |> Timeline.update (Time.millisToPosix 2000)
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
                |> Timeline.updateNoGC (Time.millisToPosix 0)
                |> Animator.interrupt
                    [ Animator.wait (Animator.seconds 1.0)
                    , Animator.event (Animator.seconds 1) Three
                    , Animator.wait (Animator.seconds 1.0)
                    , Animator.event (Animator.seconds 1) Four
                    ]
                |> Timeline.updateNoGC (Time.millisToPosix 3000)
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
                                    (occur Starting (qty 0) (qty 1000))
                                    [ occur One (qty 2000) (qty 3000)
                                    , occur Two (qty 4000) (qty 5000)
                                    , occur Unreachable (qty 6000) (qty 7000)
                                    ]

                                -- we scheduled at 3000
                                -- but there's a wait for 1000
                                -- so the new line actually starts at 4000.
                                , Timeline.Line (qty 4000)
                                    -- then take 1 second to transition to Three
                                    (occur Three (qty 5000) (qty 6000))
                                    -- we then wait a second
                                    -- then take a second to transition to Four
                                    [ occur Four (qty 7000) (qty 7000)
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
                        Timeline.update (Time.millisToPosix t) newTimeline
                in
                Expect.atLeast 0
                    (Animator.move new toVals)
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
                            |> Timeline.updateNoGC (Time.millisToPosix 0)
                            |> Animator.interrupt
                                [ Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) Three
                                , Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) Four
                                ]
                            |> Timeline.updateNoGC (Time.millisToPosix 3000)
                            |> Animator.interrupt
                                [ Animator.event (Animator.seconds 1) Two
                                , Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) One
                                ]
                            |> Timeline.updateNoGC (Time.millisToPosix 4500)
                in
                Expect.equal
                    doubleInterrupted
                    (Timeline.Timeline
                        { events =
                            Timeline.Timetable
                                [ Timeline.Line (qty 0)
                                    (occur Starting (qty 0) (qty 1000))
                                    [ occur One (qty 2000) (qty 3000)
                                    , occur Two (qty 4000) (qty 5000)
                                    , occur Unreachable (qty 6000) (qty 7000)
                                    ]

                                -- we scheduled at 3000
                                -- but there's a wait for 1000
                                -- so the new line actually starts at 4000.
                                , Timeline.Line (qty 4000)
                                    -- then take 1 second to transition to Three
                                    (occur Three (qty 5000) (qty 6000))
                                    -- we then wait a second
                                    -- then take a second to transition to Four
                                    [ occur Four (qty 7000) (qty 7000)
                                    ]
                                , Timeline.Line (qty 4500)
                                    (occur Two (qty 5500) (qty 6500))
                                    [ occur One (qty 7500) (qty 7500)
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
                            |> Timeline.updateNoGC (Time.millisToPosix 0)

                    interruptedAfterFinish =
                        baseline
                            |> Animator.interrupt
                                [ Animator.event (Animator.seconds 1) Four
                                , Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) Five
                                ]
                            |> Timeline.updateNoGC (Time.millisToPosix 6000)
                in
                Expect.equal
                    interruptedAfterFinish
                    (Timeline.Timeline
                        { events =
                            Timeline.Timetable
                                [ Timeline.Line (qty 0)
                                    (occur Starting (qty 0) (qty 1000))
                                    [ occur One (qty 2000) (qty 3000)
                                    , occur Two (qty 4000) (qty 5000)
                                    , occur Three (qty 6000) (qty 6000)
                                    , occur Four (qty 7000) (qty 8000)
                                    , occur Five (qty 9000) (qty 9000)
                                    ]
                                ]
                        , initial = Starting
                        , interruption = []
                        , now = qty 6000
                        , queued = Nothing
                        , running = True
                        }
                    )
        , test "Progress, halfway" <|
            \_ ->
                let
                    prog =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.queue
                                [ Animator.event (Animator.seconds 1) One
                                ]
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 500)
                            |> Timeline.progress
                in
                Expect.within
                    (Absolute 0.001)
                    prog
                    0.5
        , test "Progress, resting at end" <|
            \_ ->
                let
                    prog =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.queue
                                [ Animator.event (Animator.seconds 1) One
                                ]
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 1200)
                            |> Timeline.progress
                in
                Expect.within
                    (Absolute 0.001)
                    prog
                    1.0
        , test "Progress, with interruption." <|
            \_ ->
                let
                    prog =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.queue
                                [ Animator.event (Animator.seconds 1) One
                                ]
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.interrupt
                                [ Animator.event (Animator.seconds 1) Two
                                ]
                            |> Timeline.update (Time.millisToPosix 500)
                            |> Timeline.update (Time.millisToPosix 1000)
                            |> Timeline.progress
                in
                Expect.within
                    (Absolute 0.001)
                    prog
                    0.5
        , test "Dwelling time, resting at end" <|
            \_ ->
                let
                    foundDwellTime =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.queue
                                [ Animator.event (Animator.seconds 1) One
                                ]
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 1200)
                            |> Timeline.dwellingTime
                in
                Expect.within
                    (Absolute 0.001)
                    foundDwellTime
                    200
        , test "Dwelling time, transitioning" <|
            \_ ->
                let
                    foundDwellTime =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.queue
                                [ Animator.event (Animator.seconds 1) One
                                ]
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Timeline.update (Time.millisToPosix 500)
                            |> Timeline.dwellingTime
                in
                Expect.within
                    (Absolute 0.001)
                    foundDwellTime
                    0
        , test "Dwelling time, with interruption." <|
            \_ ->
                let
                    foundDwellTime =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.queue
                                [ Animator.event (Animator.seconds 1) One
                                , Animator.wait (Animator.seconds 1)
                                ]
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.interrupt
                                [ Animator.event (Animator.seconds 1) Two
                                ]
                            |> Timeline.update (Time.millisToPosix 500)
                            |> Timeline.update (Time.millisToPosix 1000)
                            |> Timeline.dwellingTime
                in
                Expect.within
                    (Absolute 0.001)
                    foundDwellTime
                    0
        ]


cleaning =
    describe "Cleaning"
        [ test "Marked as running correctly" <|
            \_ ->
                let
                    lines =
                        [ Timeline.Line
                            (qty 1578168889621)
                            (occur False (qty 1578168889621) (qty 1578168889621))
                            [ occur True (qty 1578168895231) (qty 1578168895231)
                            ]
                        , Timeline.Line
                            -- same as now
                            (qty 1578168893838)
                            -- 1000ms later
                            (occur False (qty 1578168895838) (qty 1578168895838))
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
                            (occur False (qty 1578168889621) (qty 1578168889621))
                            [ occur True (qty 1578168895231) (qty 1578168895231)
                            ]
                        , Timeline.Line
                            -- same as now
                            (qty 1578168893838)
                            -- 1000ms later
                            (occur False (qty 1578168895838) (qty 1578168895838))
                            []
                        ]

                    now =
                        qty 1578168893839
                in
                Expect.true "This timeline at this time should still be active"
                    (Timeline.linesAreActive now lines)
        , test "Reduce down timeline to event we're dwelling on." <|
            \_ ->
                let
                    newTimeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.queue
                                [ Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) One
                                , Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) Two
                                ]
                            |> Timeline.update (Time.millisToPosix 1000)
                            |> Timeline.update (Time.millisToPosix 5000)
                            |> Timeline.gc
                in
                Expect.equal
                    newTimeline
                    (Timeline.Timeline
                        { events =
                            Timeline.Timetable
                                [ Timeline.Line (qty 5000)
                                    (occur Two (qty 5000) (qty 5000))
                                    []
                                ]
                        , initial = Starting
                        , interruption = []
                        , now = qty 5000
                        , queued = Nothing
                        , running = True
                        }
                    )
        , test "Reduce multiple timelines down if we're dwelling" <|
            \_ ->
                let
                    newTimeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.queue
                                [ Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) One
                                , Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) Two
                                ]
                            |> Timeline.update (Time.millisToPosix 1000)
                            |> Animator.interrupt
                                [ Animator.event (Animator.seconds 1) Four
                                , Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) Five
                                ]
                            |> Timeline.update (Time.millisToPosix 2000)
                            |> Timeline.update (Time.millisToPosix 5000)
                            |> Timeline.gc
                in
                Expect.equal
                    newTimeline
                    (Timeline.Timeline
                        { events =
                            Timeline.Timetable
                                [ Timeline.Line (qty 5000)
                                    (occur Five (qty 5000) (qty 5000))
                                    []
                                ]
                        , initial = Starting
                        , interruption = []
                        , now = qty 5000
                        , queued = Nothing
                        , running = True
                        }
                    )
        , test "Unknown GC event" <|
            \_ ->
                let
                    newTimeline =
                        Animator.init Starting
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.queue
                                [ Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) One
                                , Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) Two
                                ]
                            |> Timeline.update (Time.millisToPosix 1000)
                            |> Animator.interrupt
                                [ Animator.event (Animator.seconds 1) Four
                                , Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) Five
                                ]
                            |> Timeline.update (Time.millisToPosix 2000)
                            |> Timeline.update (Time.millisToPosix 5000)
                            |> Timeline.gc
                in
                Expect.equal
                    newTimeline
                    (Timeline.Timeline
                        { events =
                            Timeline.Timetable
                                [ Timeline.Line (qty 5000)
                                    (occur Five (qty 5000) (qty 5000))
                                    []
                                ]
                        , initial = Starting
                        , interruption = []
                        , now = qty 5000
                        , queued = Nothing
                        , running = True
                        }
                    )
        ]


tailRecursion =
    describe "Tail recursion"
        [ test "Enqueueing" <|
            \_ ->
                let
                    newTimeline =
                        Animator.init 0
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.queue
                                (List.map (Animator.event (Animator.seconds 1)) (List.range 0 10000))
                            |> Timeline.update (Time.millisToPosix 5000)
                in
                Expect.true "Successfully enqueued 10,000 events" True
        , test "Interrupting" <|
            \_ ->
                let
                    newTimeline =
                        Animator.init 0
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.interrupt
                                (List.map (Animator.event (Animator.seconds 1)) (List.range 0 10000))
                            |> Timeline.update (Time.millisToPosix 5000)
                in
                Expect.true "Successfully interupt with 10,000 events" True
        , test "Interpolating" <|
            \_ ->
                let
                    newTimeline =
                        Animator.init 0
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.queue
                                (List.map (Animator.event (Animator.seconds 1)) (List.range 0 10000))
                            |> Timeline.update (Time.millisToPosix 5000)

                    now =
                        Interpolate.details
                            (Timeline.atTime
                                (Time.millisToPosix 1000000)
                                newTimeline
                            )
                            (\x -> Animator.at (toFloat x))
                in
                Expect.true "Successfully interpolate with 10,000 events" True
        ]


ordering =
    describe "Preserve Ordering"
        [ fuzz (Fuzz.Timeline.timeline 0 6000 [ One, Two, Three, Four, Five ]) "Line order is always preserved" <|
            \timelineInstruction ->
                let
                    actualTimeline =
                        Fuzz.Timeline.toTimeline { gc = True } timelineInstruction
                in
                case actualTimeline of
                    Timeline.Timeline details ->
                        case details.events of
                            Timeline.Timetable lines ->
                                let
                                    order =
                                        List.foldl isOrderPreserved ( 0, True ) lines
                                in
                                Expect.true "Line order is not preserved"
                                    (Tuple.second order)
        , test "Line order test case 1" <|
            \_ ->
                let
                    instructions =
                        Fuzz.Timeline.InstructionTimeline 0
                            Five
                            [ Fuzz.Timeline.Queue 1 [ ( 1, Two ) ]
                            , Fuzz.Timeline.Interruption 1 [ ( 1, One ) ]
                            , Fuzz.Timeline.Interruption 1 [ ( 0, Four ) ]
                            , Fuzz.Timeline.Interruption 2 [ ( 0, Two ) ]
                            , Fuzz.Timeline.Interruption 2 [ ( 0, Five ) ]
                            ]

                    actualTimeline =
                        Fuzz.Timeline.toTimeline { gc = False } instructions
                in
                case actualTimeline of
                    Timeline.Timeline details ->
                        case details.events of
                            Timeline.Timetable lines ->
                                let
                                    order =
                                        List.foldl isOrderPreserved ( 0, True ) lines
                                in
                                Expect.true "Line order is not preserved"
                                    (Tuple.second order)
        , test "Line order test case 2" <|
            \_ ->
                let
                    instructions =
                        Fuzz.Timeline.InstructionTimeline 0
                            Five
                            [ Fuzz.Timeline.Interruption 0 [ ( 1, Two ) ]
                            , Fuzz.Timeline.Interruption 0 [ ( 1, Four ) ] --, ( 0, Four ), ( 0, One ), ( 0, One ) ]
                            , Fuzz.Timeline.Interruption 0 [ ( 0, One ) ]
                            , Fuzz.Timeline.Interruption 1 [ ( 0, Three ) ] --, ( 0, One ), ( 0, Five ), ( 0, Five ), ( 0, Two ) ]
                            ]

                    actualTimeline =
                        Fuzz.Timeline.toTimeline { gc = False } instructions
                in
                case actualTimeline of
                    Timeline.Timeline details ->
                        case details.events of
                            Timeline.Timetable lines ->
                                let
                                    order =
                                        List.foldl isOrderPreserved ( 0, True ) lines
                                in
                                Expect.true "Line order is not preserved"
                                    (Tuple.second order)
        , fuzz (Fuzz.Timeline.timeline 0 6000 [ One, Two, Three, Four, Five ])
            "Event order is always preserved"
          <|
            \timelineInstruction ->
                case Fuzz.Timeline.toTimeline { gc = True } timelineInstruction of
                    Timeline.Timeline details ->
                        case details.events of
                            Timeline.Timetable lines ->
                                let
                                    preserved =
                                        List.all isEventOrderPreserved lines
                                in
                                Expect.true "Event order is preserved"
                                    preserved
        , fuzz (Fuzz.Timeline.timeline 0 6000 [ One, Two, Three, Four, Five ])
            "GC doesn't affect order"
          <|
            \timelineInstruction ->
                case Fuzz.Timeline.toTimeline { gc = True } timelineInstruction of
                    Timeline.Timeline details ->
                        case details.events of
                            Timeline.Timetable lines ->
                                Expect.true "Event order after GC is preserved"
                                    (List.all isEventOrderPreserved lines)
        , fuzz (Fuzz.Timeline.timeline 0 6000 [ One, Two, Three, Four, Five ])
            "GC is idempotent"
          <|
            \timelineInstruction ->
                let
                    actualTimeline =
                        Fuzz.Timeline.toTimeline { gc = True } timelineInstruction

                    gcedTimeline =
                        Timeline.gc actualTimeline
                in
                Expect.equal
                    (Timeline.gc actualTimeline)
                    (Timeline.gc gcedTimeline)
        , fuzz (Fuzz.Timeline.timeline 0 6000 [ One, Two, Three, Four, Five ])
            "GC does not affect values at and after gc time."
          <|
            \timelineInstruction ->
                let
                    time =
                        Time.millisToPosix 1400

                    actualTimeline =
                        Fuzz.Timeline.toTimeline { gc = False } timelineInstruction

                    timelineAt =
                        Timeline.atTime time actualTimeline

                    gcedTimeline =
                        Timeline.gc timelineAt
                in
                Expect.all
                    [ \tl ->
                        Expect.within
                            (Absolute 0.001)
                            (.position (Interpolate.details tl toPosition))
                            (.position (Interpolate.details (Timeline.gc tl) toPosition))
                    , \tl ->
                        Expect.within
                            (Absolute 0.001)
                            (.velocity (Interpolate.details tl toPosition))
                            (.velocity (Interpolate.details (Timeline.gc tl) toPosition))
                    ]
                    timelineAt
        , test "Harmless GC, test case 1" <|
            \_ ->
                let
                    time =
                        Time.millisToPosix 1200

                    instructions =
                        Fuzz.Timeline.InstructionTimeline 0
                            One
                            [ Fuzz.Timeline.Queue 0
                                [ ( 0, Four )
                                ]
                            , Fuzz.Timeline.Interruption 1
                                [ ( 0, One )
                                , ( 0, Three )
                                , ( 0, Two )
                                ]
                            ]

                    actualTimeline =
                        Fuzz.Timeline.toTimeline { gc = False } instructions

                    timelineAt =
                        Timeline.atTime time actualTimeline

                    gcedTimeline =
                        Timeline.gc timelineAt
                in
                Expect.all
                    [ \tl ->
                        Expect.within
                            (Absolute 0.001)
                            (.position (Interpolate.details tl toPosition))
                            (.position (Interpolate.details (Timeline.gc tl) toPosition))
                    , \tl ->
                        Expect.within
                            (Absolute 0.001)
                            (.velocity (Interpolate.details tl toPosition))
                            (.velocity (Interpolate.details (Timeline.gc tl) toPosition))
                    ]
                    timelineAt
        , fuzz (Fuzz.Timeline.timeline 0 6000 [ One, Two, Three, Four, Five ])
            "Value is never NaN."
          <|
            \timelineInstruction ->
                let
                    time =
                        Time.millisToPosix 1400

                    actualTimeline =
                        Fuzz.Timeline.toTimeline { gc = False } timelineInstruction

                    timelineAt =
                        Timeline.atTime time actualTimeline

                    movement =
                        Interpolate.details timelineAt toPosition
                in
                Expect.true "Is NaN"
                    (not (isNaN movement.position))
        , test "GC trims down a single line if necessary" <|
            \_ ->
                let
                    newTimeline =
                        Animator.init 0
                            |> Timeline.update (Time.millisToPosix 0)
                            |> Animator.queue
                                (List.map
                                    (Animator.event (Animator.seconds 1))
                                    (List.range 0 10000)
                                )
                            |> Timeline.update (Time.millisToPosix 5000)
                            |> Timeline.update (Time.millisToPosix (500 * 1000))

                    eventCount =
                        case newTimeline of
                            Timeline.Timeline details ->
                                case details.events of
                                    Timeline.Timetable lines ->
                                        case lines of
                                            [] ->
                                                0

                                            (Timeline.Line _ _ evs) :: _ ->
                                                List.length evs
                in
                -- we are jsut testing that previous events are being removed
                -- we don't really care how many.
                -- but it should be ~500 in this case.
                Expect.equal
                    eventCount
                    9506
        ]


toPosition event =
    case event of
        Starting ->
            Animator.at 0

        One ->
            Animator.at 1

        Two ->
            Animator.at 2

        Three ->
            Animator.at 3

        Four ->
            Animator.at 4

        Five ->
            Animator.at 5

        Unreachable ->
            Animator.at -1


isOrderPreserved (Timeline.Line start _ _) (( previous, preserved ) as existing) =
    if not preserved then
        existing

    else
        let
            newPrevious =
                Time.posixToMillis (Time.toPosix start)
        in
        ( newPrevious
        , newPrevious >= previous
        )


isEventOrderPreserved (Timeline.Line start startingEvent events) =
    let
        orderPreserved event (( prev, preserved ) as skip) =
            if not preserved then
                skip

            else
                let
                    newPrev =
                        Timeline.startTime event
                            |> Time.toPosix
                            |> Time.posixToMillis
                in
                ( newPrev
                , newPrev >= prev
                )
    in
    List.foldl orderPreserved ( Time.posixToMillis (Time.toPosix start), True ) (startingEvent :: events)
        |> Tuple.second



-- describeInstructions =
--     Debug.todo ""


skipLog str x =
    let
        _ =
            Debug.log str ""
    in
    x
