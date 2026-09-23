module Values exposing (suite)

import Animator
import Animator.Timeline as Timeline
import Animator.Transition as Transition
import Animator.Value as Value
import Color
import Expect
import Fuzz
import Test exposing (Test, describe, fuzz3, test)
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
        ]


linear : Float -> Value.Movement
linear value =
    Value.to value |> Value.withTransition Transition.linear
