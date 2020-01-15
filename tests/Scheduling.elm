module Scheduling exposing (cleaning, interruptions, ordering, queueing, tailRecursion)

import Animator
import Duration
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float, int, list, string)
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
            -- we can't call update because updates have to be monotonic
            Timeline.atTime (Time.millisToPosix time) tl
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
                                    (occur Starting (qty 0) (Just (qty 1)))
                                    [ occur One (qty 2000) (Just (qty 1))
                                    , occur Two (qty 4000) (Just (qty 1))
                                    , occur Three (qty 6000) Nothing
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
                            |> Timeline.updateNoGC (Time.millisToPosix 0)

                    interruptedAfterFinish =
                        baseline
                            |> Animator.interrupt
                                [ Animator.event (Animator.seconds 1) Four
                                , Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) Five
                                ]
                            |> Timeline.updateNoGC (Time.millisToPosix 8000)
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
        , test "Reduce down timeline to event we're dwelling on." <|
            \_ ->
                let
                    newTimeline =
                        Animator.init Starting
                            |> Animator.update (Time.millisToPosix 0)
                            |> Animator.queue
                                [ Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) One
                                , Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) Two
                                ]
                            |> Animator.update (Time.millisToPosix 1000)
                            |> Animator.update (Time.millisToPosix 5000)
                            |> Timeline.gc
                in
                Expect.equal
                    newTimeline
                    (Timeline.Timeline
                        { events =
                            Timeline.Timetable
                                [ Timeline.Line (qty 5000)
                                    (occur Two (qty 5000) Nothing)
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
                            |> Animator.update (Time.millisToPosix 0)
                            |> Animator.queue
                                [ Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) One
                                , Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) Two
                                ]
                            |> Animator.update (Time.millisToPosix 1000)
                            |> Animator.interrupt
                                [ Animator.event (Animator.seconds 1) Four
                                , Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) Five
                                ]
                            |> Animator.update (Time.millisToPosix 2000)
                            |> Animator.update (Time.millisToPosix 5000)
                            |> Timeline.gc
                in
                Expect.equal
                    newTimeline
                    (Timeline.Timeline
                        { events =
                            Timeline.Timetable
                                [ Timeline.Line (qty 5000)
                                    (occur Five (qty 5000) Nothing)
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
                            |> Animator.update (Time.millisToPosix 0)
                            |> Animator.queue
                                [ Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) One
                                , Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) Two
                                ]
                            |> Animator.update (Time.millisToPosix 1000)
                            |> Animator.interrupt
                                [ Animator.event (Animator.seconds 1) Four
                                , Animator.wait (Animator.seconds 1.0)
                                , Animator.event (Animator.seconds 1) Five
                                ]
                            |> Animator.update (Time.millisToPosix 2000)
                            |> Animator.update (Time.millisToPosix 5000)
                            |> Timeline.gc
                in
                Expect.equal
                    newTimeline
                    (Timeline.Timeline
                        { events =
                            Timeline.Timetable
                                [ Timeline.Line (qty 5000)
                                    (occur Five (qty 5000) Nothing)
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
                            |> Animator.update (Time.millisToPosix 0)
                            |> Animator.queue
                                (List.map (Animator.event (Animator.seconds 1)) (List.range 0 10000))
                            |> Animator.update (Time.millisToPosix 5000)
                in
                Expect.true "Successfully enqueued 10,000 events" True
        , test "Interrupting" <|
            \_ ->
                let
                    newTimeline =
                        Animator.init 0
                            |> Animator.update (Time.millisToPosix 0)
                            |> Animator.interrupt
                                (List.map (Animator.event (Animator.seconds 1)) (List.range 0 10000))
                            |> Animator.update (Time.millisToPosix 5000)
                in
                Expect.true "Successfully interupt with 10,000 events" True
        , test "Interpolating" <|
            \_ ->
                let
                    newTimeline =
                        Animator.init 0
                            |> Animator.update (Time.millisToPosix 0)
                            |> Animator.queue
                                (List.map (Animator.event (Animator.seconds 1)) (List.range 0 10000))
                            |> Animator.update (Time.millisToPosix 5000)

                    now =
                        Animator.move
                            (Timeline.atTime
                                (Time.millisToPosix 1000000)
                                newTimeline
                            )
                            (\x -> Animator.to (toFloat x))
                in
                Expect.true "Successfully interpolate with 10,000 events" True
        ]


ordering =
    describe "Preserve Ordering"
        [ fuzz (fuzzTimeline 0 6000 [ One, Two, Three, Four, Five ]) "Line order is always preserved" <|
            \timelineInstruction ->
                let
                    actualTimeline =
                        instructionsToTimeline timelineInstruction
                in
                case actualTimeline of
                    Timeline.Timeline details ->
                        case details.events of
                            Timeline.Timetable lines ->
                                let
                                    order =
                                        List.foldl isOrderPreserved ( 0, True ) lines
                                in
                                Expect.true "Line order is preserved"
                                    (Tuple.second order)
        , fuzz (fuzzTimeline 0 6000 [ One, Two, Three, Four, Five ])
            "Event order is always preserved"
          <|
            \timelineInstruction ->
                case instructionsToTimeline timelineInstruction of
                    Timeline.Timeline details ->
                        case details.events of
                            Timeline.Timetable lines ->
                                let
                                    preserved =
                                        List.all isEventOrderPreserved lines
                                in
                                Expect.true "Event order is preserved"
                                    preserved
        , fuzz (fuzzTimeline 0 6000 [ One, Two, Three, Four, Five ])
            "GC doesn't affect order"
          <|
            \timelineInstruction ->
                case instructionsToTimeline timelineInstruction of
                    Timeline.Timeline details ->
                        case details.events of
                            Timeline.Timetable lines ->
                                Expect.true "Event order after GC is preserved"
                                    (List.all isEventOrderPreserved lines)
        , fuzz (fuzzTimeline 0 6000 [ One, Two, Three, Four, Five ])
            "GC is idempotent"
          <|
            \timelineInstruction ->
                let
                    actualTimeline =
                        instructionsToTimeline timelineInstruction

                    gcedTimeline =
                        Timeline.gc actualTimeline
                in
                Expect.equal
                    actualTimeline
                    (Timeline.gc gcedTimeline)
        ]


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



{- FUZZ TESTING FOR TIMELINES




   Ordering
       1. No matter what set of interruptions or queues, the timeline order is correct.
       2. Same with event order within timelines.

   GC
       1. GC does not affect 'observable' events
           - An event is observable If it's in an unbroken chain of interruption/queued events tilll `Now`
           - i.e. An event that dwells can drop itself and all previous events.


   Timeline Fuzzer:

       - Add some number of Queue and Interrupt at random times on a range.
       - Specific start and end time(the range)

       - High level description of pipeline.
       - From description ->
               - Calc observable timeline
               - Create actual timeline


-}


type InstructionTimeline event
    = InstructionTimeline Int event (List (Instruction event))


type Instruction event
    = Queue Int (List ( Int, event ))
    | Interruption Int (List ( Int, event ))


fuzzTimeline : Int -> Int -> List event -> Fuzzer (InstructionTimeline event)
fuzzTimeline one two eventOptions =
    let
        start =
            min one two

        end =
            max one two

        timeRange =
            Fuzz.intRange start end

        durationRange =
            Fuzz.intRange 0 ((end - start) // 3)

        instructionFuzzer =
            listOneToFive
                (Fuzz.oneOf
                    [ Fuzz.map2 Interruption timeRange (fuzzEvents durationRange eventOptions)
                    , Fuzz.map2 Queue timeRange (fuzzEvents durationRange eventOptions)
                    ]
                )
    in
    Fuzz.map2
        (\startingEvent instructions ->
            InstructionTimeline start startingEvent instructions
        )
        (Fuzz.oneOf (List.map Fuzz.constant eventOptions))
        instructionFuzzer


fuzzEvents : Fuzzer Int -> List event -> Fuzzer (List ( Int, event ))
fuzzEvents timeRange eventOptions =
    listOneToFive
        (Fuzz.map2 Tuple.pair
            timeRange
            (Fuzz.oneOf (List.map Fuzz.constant eventOptions))
        )


instructionsToTimeline : InstructionTimeline event -> Timeline.Timeline event
instructionsToTimeline (InstructionTimeline startTime startEvent instructions) =
    let
        addInstructions myTimeline =
            List.foldl instruct myTimeline instructions

        instruct instruction myTimeline =
            case instruction of
                Queue start events ->
                    myTimeline
                        |> Animator.queue (List.map instructionToEvent events)
                        |> Animator.update (Time.millisToPosix start)

                Interruption start events ->
                    myTimeline
                        |> Animator.interrupt (List.map instructionToEvent events)
                        |> Animator.update (Time.millisToPosix start)
    in
    Animator.init startEvent
        |> Animator.update (Time.millisToPosix startTime)
        |> addInstructions


instructionToEvent ( i, event ) =
    Animator.event (Animator.millis (toFloat i)) event


listOneToFive : Fuzzer a -> Fuzzer (List a)
listOneToFive contents =
    Fuzz.oneOf
        [ Fuzz.map
            List.singleton
            contents
        , Fuzz.map2
            (\one two ->
                [ one, two ]
            )
            contents
            contents
        , Fuzz.map3
            (\one two three ->
                [ one, two, three ]
            )
            contents
            contents
            contents
        , Fuzz.map4
            (\one two three four ->
                [ one, two, three, four ]
            )
            contents
            contents
            contents
            contents
        , Fuzz.map5
            (\one two three four five ->
                [ one, two, three, four, five ]
            )
            contents
            contents
            contents
            contents
            contents
        ]



-- describeInstructions =
--     Debug.todo ""


skipLog str x =
    let
        _ =
            Debug.log str ""
    in
    x
