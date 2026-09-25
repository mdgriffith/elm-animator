module TimelineRetention exposing (suite)

import Animator as Anim
import Animator.Timeline as Timeline
import Animator.Transition as Transition
import Animator.Value as Value
import Expect
import Fuzz
import Fuzz.Timeline
import InternalAnim.Time as Clock
import InternalAnim.Timeline as Internal
import Test exposing (Test, describe, fuzz2, test)
import Time


suite : Test
suite =
    describe "Retained timeline history"
        [ test "Previous survives collection across completed lines" <|
            \_ ->
                Timeline.init "A"
                    |> Timeline.to (Anim.ms 1000) "B"
                    |> at 10000
                    |> at 11100
                    |> Timeline.to (Anim.ms 1000) "C"
                    |> at 11100
                    |> at 12100
                    |> at 20000
                    |> Timeline.previous
                    |> Expect.equal "B"
        , test "A permitted view delay can still sample recently completed movement" <|
            \_ ->
                queued
                    |> at 13500
                    |> Timeline.delay (Anim.ms 2500)
                    |> position
                    |> Expect.within (Expect.Absolute 0.1) 100
        , test "The oldest supported delayed sample survives an actual compaction" <|
            \_ ->
                queued
                    |> at 18000
                    |> Timeline.delay (Anim.ms 5000)
                    |> Expect.all
                        [ position >> Expect.within (Expect.Absolute 0.1) 300
                        , Timeline.arrived >> Expect.equal 300
                        , Timeline.previous >> Expect.equal 400
                        ]
        , test "Repeated collection keeps previous and arrived stable after all motion ends" <|
            \_ ->
                queued
                    |> at 20000
                    |> at 30000
                    |> at 40000
                    |> Expect.all
                        [ Timeline.previous >> Expect.equal 400
                        , Timeline.arrived >> Expect.equal 300
                        , position >> Expect.within (Expect.Absolute 0.001) 300
                        ]
        , fuzz2 (Fuzz.Timeline.timeline 0 20000 [ 0, 100, 400, -50 ])
            (Fuzz.intRange 0 5000)
            "Automatic collection preserves all inspectors and movement throughout the delay window"
          <|
            \instructions delay ->
                let
                    uncollected =
                        Fuzz.Timeline.toTimeline { gc = False } instructions

                    collected =
                        Fuzz.Timeline.toTimeline { gc = True } instructions

                    later =
                        Clock.advanceBy (Anim.ms 6000) (Internal.getCurrentTime uncollected)
                            |> Clock.toPosix

                    compare before after =
                        let
                            expected =
                                Timeline.delay (Anim.ms (toFloat delay)) before

                            actual =
                                Timeline.delay (Anim.ms (toFloat delay)) after

                            expectedMotion =
                                movement expected
                        in
                        Expect.all
                            [ \_ -> Expect.equal (inspect expected) (inspect actual)
                            , \_ -> Expect.within (Expect.Absolute 0.000001) (Timeline.progress expected) (Timeline.progress actual)
                            , \_ -> Expect.within (Expect.Absolute 0.001) expectedMotion.position (movement actual).position
                            , \_ -> Expect.within (Expect.Absolute 0.001) expectedMotion.velocity (movement actual).velocity
                            , \_ -> Expect.equal (List.map (\value -> Timeline.upcoming value expected) [ 0, 100, 400, -50 ]) (List.map (\value -> Timeline.upcoming value actual) [ 0, 100, 400, -50 ])
                            ]
                            ()
                in
                Expect.all
                    [ \_ -> compare uncollected collected
                    , \_ -> compare (Internal.updateWith False later uncollected) (Timeline.update later collected)
                    ]
                    ()
        ]


queued : Timeline.Timeline Float
queued =
    Timeline.init 0
        |> Timeline.queue
            [ Timeline.transitionTo (Anim.ms 1000) 100
            , Timeline.transitionTo (Anim.ms 1000) 400
            , Timeline.transitionTo (Anim.ms 1000) 300
            , Timeline.wait (Anim.ms 1000)
            ]
        |> at 10000


at : Int -> Timeline.Timeline state -> Timeline.Timeline state
at millis =
    Timeline.update (Time.millisToPosix millis)


movement : Timeline.Timeline Float -> { position : Float, velocity : Float }
movement timeline =
    Value.movement timeline (Value.to >> Value.withTransition Transition.linear)


position : Timeline.Timeline Float -> Float
position =
    movement >> .position


inspect : Timeline.Timeline Float -> ( Float, Float, Float )
inspect timeline =
    ( Timeline.current timeline, Timeline.arrived timeline, Timeline.previous timeline )
