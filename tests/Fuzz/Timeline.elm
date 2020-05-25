module Fuzz.Timeline exposing
    ( Instruction(..)
    , InstructionTimeline(..)
    , timeline
    , toTimeline
    )

{-| FUZZ TESTING FOR TIMELINES

Ordering

1.  No matter what set of interruptions or queues, the timeline order is correct.
2.  Same with event order within timelines.

GC

1.  GC does not affect 'observable' events
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

import Animator
import Fuzz exposing (Fuzzer, float, int, list, string)
import Internal.Interpolate as Interpolate
import Internal.Time as Time
import Internal.Timeline as Timeline
import Time


type InstructionTimeline event
    = InstructionTimeline Int event (List (Instruction event))


type Instruction event
    = Queue Int (List ( Int, event ))
    | Interruption Int (List ( Int, event ))


timeline : Int -> Int -> List event -> Fuzzer (InstructionTimeline event)
timeline one two eventOptions =
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


toTimeline : { gc : Bool } -> InstructionTimeline event -> Timeline.Timeline event
toTimeline { gc } (InstructionTimeline startTime startEvent instructions) =
    let
        update x =
            if gc then
                Timeline.update x

            else
                Timeline.updateWith False x

        addInstructions myTimeline =
            List.foldl instruct myTimeline instructions

        instruct instruction myTimeline =
            case instruction of
                Queue start events ->
                    myTimeline
                        |> Animator.queue (List.map instructionToEvent events)
                        |> update (Time.millisToPosix start)

                Interruption start events ->
                    myTimeline
                        |> Animator.interrupt (List.map instructionToEvent events)
                        |> update (Time.millisToPosix start)
    in
    Animator.init startEvent
        |> update (Time.millisToPosix startTime)
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
