module TimelineLaws exposing (suite)

import Animator
import Animator.Timeline as Timeline
import Animator.Transition as Transition
import Animator.Value as Value
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz2, fuzz3, test)
import Time


suite : Test
suite =
    describe "Public scheduling laws"
        [ test "Scheduling changes are pending until the next update" <|
            \_ ->
                let
                    initial =
                        Timeline.init 0

                    queued =
                        Timeline.queue [ Timeline.transitionTo (Animator.ms 1000) 100 ] initial

                    interrupted =
                        Timeline.to (Animator.ms 1000) 200 initial
                in
                Expect.equal
                    [ False, True, False, True, False ]
                    (List.map Timeline.hasChanges [ initial, queued, at 0 queued, interrupted, at 0 interrupted ])
        , test "Finished timelines stop requesting animation frames" <|
            \_ ->
                Timeline.init 0
                    |> Timeline.to (Animator.ms 1000) 100
                    |> at 0
                    |> at 1001
                    |> Timeline.isRunning
                    |> Expect.equal False
        , test "The latest interruption before an update wins" <|
            \_ ->
                Timeline.init 0
                    |> Timeline.to (Animator.ms 1000) 100
                    |> Timeline.to (Animator.ms 1000) 200
                    |> at 0
                    |> at 1001
                    |> Timeline.arrived
                    |> Expect.equal 200
        , test "Waiting holds the value between queued transitions" <|
            \_ ->
                scheduled 1000 500 1000
                    |> at 1250
                    |> position
                    |> Expect.within (Expect.Absolute 0.001) 100
        , test "A second queued transition reports its own progress" <|
            \_ ->
                scheduled 1000 500 1000
                    |> at 2000
                    |> Timeline.progress
                    |> Expect.within (Expect.Absolute 0.001) 0.5
        , test "Progress refers to the active transition, not a later queued transition" <|
            \_ ->
                scheduled 1000 500 1000
                    |> at 500
                    |> Timeline.progress
                    |> Expect.within (Expect.Absolute 0.001) 0.5
        , fuzz3 (Fuzz.intRange 10 1000)
            (Fuzz.intRange 0 1000)
            (Fuzz.intRange 0 3000)
            "Queued movement agrees with a two-segment reference model"
          <|
            \duration wait elapsed ->
                let
                    expected =
                        if elapsed <= duration then
                            100 * toFloat elapsed / toFloat duration

                        else if elapsed <= duration + wait then
                            100

                        else
                            100 + 100 * clamp 0 1 (toFloat (elapsed - duration - wait) / toFloat duration)
                in
                scheduled duration wait duration
                    |> at elapsed
                    |> position
                    |> Expect.within (Expect.Absolute 0.1) expected
        , fuzz2 (Fuzz.intRange 0 2500)
            (Fuzz.intRange 0 2500)
            "Intermediate clock updates do not change observations at the same final time"
          <|
            \first second ->
                let
                    earlier =
                        min first second

                    later =
                        max first second

                    timeline =
                        scheduled 500 250 1000

                    direct =
                        at later timeline

                    incremental =
                        timeline |> at earlier |> at later
                in
                Expect.all
                    [ \_ -> Expect.equal (inspect direct) (inspect incremental)
                    , \_ -> Expect.within (Expect.Absolute 0.001) (position direct) (position incremental)
                    ]
                    ()
        , fuzz2 (Fuzz.intRange 0 3000)
            (Fuzz.intRange 0 3000)
            "Progress is finite and bounded"
          <|
            \first second ->
                let
                    progress =
                        scheduled 500 250 1000
                            |> at (min first second)
                            |> Timeline.to (Animator.ms 500) 300
                            |> at (min first second)
                            |> at (max first second)
                            |> Timeline.progress
                in
                Expect.all
                    [ \_ -> Expect.equal False (isNaN progress || isInfinite progress)
                    , \_ -> Expect.atLeast 0 progress
                    , \_ -> Expect.atMost 1 progress
                    ]
                    ()
        , test "Scale multiplies scheduled durations" <|
            \_ ->
                Timeline.init 0
                    |> Timeline.scale 2
                    |> Timeline.to (Animator.ms 1000) 100
                    |> at 0
                    |> at 1000
                    |> position
                    |> Expect.within (Expect.Absolute 0.001) 50
        , test "A view delay of 200ms samples 200ms earlier" <|
            \_ ->
                Timeline.init 0
                    |> Timeline.to (Animator.ms 1000) 100
                    |> at 0
                    |> at 700
                    |> Timeline.delay (Animator.ms 200)
                    |> position
                    |> Expect.within (Expect.Absolute 0.1) 50
        ]


scheduled : Int -> Int -> Int -> Timeline.Timeline Float
scheduled firstDuration wait secondDuration =
    Timeline.init 0
        |> Timeline.queue
            [ Timeline.transitionTo (Animator.ms (toFloat firstDuration)) 100
            , Timeline.wait (Animator.ms (toFloat wait))
            , Timeline.transitionTo (Animator.ms (toFloat secondDuration)) 200
            ]
        |> at 0


at : Int -> Timeline.Timeline state -> Timeline.Timeline state
at elapsed =
    Timeline.update (Time.millisToPosix (10000 + elapsed))


position : Timeline.Timeline Float -> Float
position timeline =
    Value.float timeline (\value -> Value.to value |> Value.withTransition Transition.linear)


inspect : Timeline.Timeline Float -> ( Float, Float, Float )
inspect timeline =
    ( Timeline.previous timeline, Timeline.current timeline, Timeline.arrived timeline )
