module Values exposing (suite)

import Animator
import Animator.Timeline as Timeline
import Animator.Transition as Transition
import Animator.Value as Value
import Color
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz2, fuzz3, test)
import Time


suite : Test
suite =
    describe "Public value interpolation"
        [ test "XYZ uses three independent coordinates" <|
            \_ ->
                Timeline.init { x = 10, y = 20, z = 30 }
                    |> (\tl -> Value.xyz tl (\point -> { x = Value.to point.x, y = Value.to point.y, z = Value.to point.z }))
                    |> Expect.equal { x = 10, y = 20, z = 30 }
        , test "XY preserves independent coordinates midway through motion" <|
            \_ ->
                Timeline.init { x = 10, y = 20 }
                    |> Timeline.to (Animator.ms 1000) { x = 110, y = 220 }
                    |> Timeline.update (Time.millisToPosix 10000)
                    |> Timeline.update (Time.millisToPosix 10500)
                    |> (\tl -> Value.xy tl (\point -> { x = linear point.x, y = linear point.y }))
                    |> Expect.equal { x = 60, y = 120 }
        , test "Color preserves its initial value" <|
            \_ ->
                Value.color (Timeline.init Color.red) identity
                    |> Expect.equal Color.red
        , test "Color reaches its destination after completion" <|
            \_ ->
                Timeline.init Color.red
                    |> Timeline.to (Animator.ms 1000) Color.blue
                    |> Timeline.update (Time.millisToPosix 10000)
                    |> Timeline.update (Time.millisToPosix 11001)
                    |> (\tl -> Value.color tl identity)
                    |> Expect.equal Color.blue
        , test "A zero-duration change immediately yields the destination and finite motion" <|
            \_ ->
                Timeline.init 10
                    |> Timeline.to (Animator.ms 0) 20
                    |> Timeline.update (Time.millisToPosix 10000)
                    |> (\tl -> Value.movement tl linear)
                    |> Expect.equal { position = 20, velocity = 0 }
        , fuzz3 (Fuzz.floatRange -500 500)
            (Fuzz.floatRange -500 500)
            (Fuzz.intRange 1 999)
            "Linear interpolation agrees with the analytic position and units-per-second velocity"
          <|
            \start target elapsed ->
                let
                    motion =
                        Timeline.init start
                            |> Timeline.to (Animator.ms 1000) target
                            |> Timeline.update (Time.millisToPosix 10000)
                            |> Timeline.update (Time.millisToPosix (10000 + elapsed))
                            |> (\tl -> Value.movement tl linear)

                    expected =
                        start + (target - start) * toFloat elapsed / 1000
                in
                Expect.all
                    -- Bézier inversion is approximate. Allow 0.1% of the
                    -- distance while checking velocity independently.
                    [ \_ -> Expect.within (Expect.Absolute (max 0.001 (abs (target - start) * 0.001))) expected motion.position
                    , \_ -> Expect.within (Expect.Absolute 0.001) (target - start) motion.velocity
                    ]
                    ()
        , test "An interruption continues from the sampled position, not the abandoned destination" <|
            \_ ->
                Timeline.init 0
                    |> Timeline.to (Animator.ms 1000) 1000
                    |> Timeline.update (Time.millisToPosix 10000)
                    |> Timeline.update (Time.millisToPosix 10500)
                    |> Timeline.to (Animator.ms 1000) 2000
                    |> Timeline.update (Time.millisToPosix 10500)
                    |> Timeline.update (Time.millisToPosix 11000)
                    |> (\tl -> Value.float tl linear)
                    |> Expect.within (Expect.Absolute 0.25) 1250
        , test "A completed transition stays at its destination" <|
            \_ ->
                Timeline.init 10
                    |> Timeline.to (Animator.ms 1000) 20
                    |> Timeline.update (Time.millisToPosix 10000)
                    |> Timeline.update (Time.millisToPosix 20000)
                    |> (\tl -> Value.float tl linear)
                    |> Expect.within (Expect.Absolute 0.001) 20
        , test "Completed linear motion has zero velocity" <|
            \_ ->
                moving 10 20 1000
                    |> at 1000
                    |> (\tl -> Value.movement tl linear)
                    |> Expect.equal { position = 20, velocity = 0 }
        , test "A zero-duration spring reaches its destination without NaN" <|
            \_ ->
                moving 10 20 0
                    |> (\tl -> Value.movement tl (Value.to >> Value.withTransition (Transition.spring { wobble = 1, quickness = 0 })))
                    |> Expect.equal { position = 20, velocity = 0 }
        , test "A queued instantaneous change waits until its scheduled time" <|
            \_ ->
                let
                    timeline =
                        Timeline.init 10
                            |> Timeline.queue [ Timeline.wait (Animator.ms 500), Timeline.transitionTo (Animator.ms 0) 20 ]
                            |> at 0
                in
                Expect.equal [ 10, 20, 20 ]
                    (List.map (\time -> Value.float (at time timeline) linear) [ 499, 500, 501 ])
        , test "A zero-duration color change immediately reaches its destination" <|
            \_ ->
                moving Color.red Color.blue 0
                    |> (\tl -> Value.color tl identity)
                    |> Expect.equal Color.blue
        , test "An interrupted color transition starts from its sampled color" <|
            \_ ->
                moving (Color.rgb 1 0 0) (Color.rgb 0 0 1) 1000
                    |> at 500
                    |> Timeline.to (Animator.ms 1000) (Color.rgb 0 1 0)
                    |> at 500
                    |> at 1000
                    |> (\tl -> Value.color tl identity)
                    |> Color.toRgba
                    |> Expect.all
                        [ .red >> Expect.within (Expect.Absolute 0.000001) 0.5
                        , .green >> Expect.within (Expect.Absolute 0.000001) (sqrt 0.5)
                        , .blue >> Expect.within (Expect.Absolute 0.000001) 0.5
                        ]
        , test "A spring interruption preserves incoming velocity at the interruption instant" <|
            \_ ->
                moving 0 200 1000
                    |> at 500
                    |> Timeline.to (Animator.ms 1000) 100
                    |> at 500
                    |> (\tl ->
                            Value.movement tl
                                (\value ->
                                    if value == 200 then
                                        linear value

                                    else
                                        Value.to value |> Value.withTransition (Transition.spring { wobble = 1, quickness = 0 })
                                )
                       )
                    |> Expect.equal { position = 100, velocity = 200 }
        , fuzz2 (Fuzz.intRange 1 999)
            (Fuzz.intRange 1 999)
            "Interrupted linear movement agrees with an independent two-segment model"
          <|
            \interrupted elapsed ->
                let
                    expected =
                        toFloat interrupted + (2000 - toFloat interrupted) * toFloat elapsed / 1000
                in
                moving 0 1000 1000
                    |> at interrupted
                    |> Timeline.to (Animator.ms 1000) 2000
                    |> at interrupted
                    |> at (interrupted + elapsed)
                    |> (\tl -> Value.float tl linear)
                    -- Account for Bézier inversion in both segments.
                    |> Expect.within (Expect.Absolute 2) expected
        ]


moving : state -> state -> Float -> Timeline.Timeline state
moving initial target duration =
    Timeline.init initial |> Timeline.to (Animator.ms duration) target |> at 0


at : Int -> Timeline.Timeline state -> Timeline.Timeline state
at elapsed =
    Timeline.update (Time.millisToPosix (10000 + elapsed))


linear : Float -> Value.Movement
linear value =
    Value.to value |> Value.withTransition Transition.linear
