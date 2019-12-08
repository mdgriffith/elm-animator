module Scheduling exposing (queueing)

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
    Animator.init (Time.millisToPosix 0) Starting
        |> Animator.update (Time.millisToPosix 0)


scheduling =
    only <|
        describe "Interruptions and"
            [ queueing

            -- , interruptions
            ]


qty =
    Quantity.Quantity


occur =
    Timeline.Occurring


queueing =
    only <|
        describe "Queueing"
            [ test "Simple queue" <|
                \_ ->
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

            -- , test "Resulting timeline foldp's correctly" <|
            --     \_ ->
            --         Debug.todo ""
            ]



-- interruptions =
--     describe "Interruptions"
--         [ test "Correctly schedules" <|
--             \_ ->
--                 Debug.todo ""
--         , test "Resulting timeline foldp's correctly" <|
--             \_ ->
--                 Debug.todo ""
--         ]
