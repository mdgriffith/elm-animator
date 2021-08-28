module Benchmarks exposing (suite)

import Animator
import Animator.Css
import Animator.Css2
import Array
import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Internal.Bezier as Bezier
import Internal.Bits as Bits
import Internal.Css as Css
import Internal.Css.Props
import Internal.Interpolate as Interpolate
import Internal.Spring as Spring
import Internal.Time as Time
import Internal.Timeline as Timeline
import Pixels
import Random
import Time


suite =
    Benchmark.describe "Animator benchmarks"
        [ --     springs
          -- , randomness
          -- , basicInterpolation
          -- , floatComparison
          -- , interpolationComponents
          --   foldp
          --   equalityCompare
          --   cssGeneration
          stringifying
        ]



-- foldp =
--     let
--         timeline =
--             Animator.init Hufflepuff
--                 |> Animator.queue
--                     [ Animator.wait (Animator.seconds 1)
--                     , Animator.transitionTo (Animator.seconds 1) Griffyndor
--                     , Animator.wait (Animator.seconds 1)
--                     , Animator.transitionTo (Animator.seconds 1) Slytherin
--                     , Animator.wait (Animator.seconds 1)
--                     , Animator.transitionTo (Animator.seconds 1) Ravenclaw
--                     , Animator.wait (Animator.seconds 1)
--                     ]
--                 |> Timeline.update (Time.millisToPosix 0)
--                 |> Timeline.update (Time.millisToPosix 1400)
--     in
--     describe "foldp"
--         [ benchmark "new foldp" <|
--             \_ ->
--                 Timeline.foldp
--                     identity
--                     { start =
--                         \_ ->
--                             Hufflepuff
--                     , dwellPeriod = \_ -> Nothing
--                     , adjustor =
--                         \_ ->
--                             Timeline.linearDefault
--                     , visit =
--                         \lookup target targetTime maybeLookAhead state ->
--                             Timeline.getEvent target
--                     , lerp =
--                         \_ maybePrevious target _ _ _ state ->
--                             target
--                     }
--                     timeline
--         , benchmark "new all foldp" <|
--             \_ ->
--                 Timeline.foldpAll
--                     identity
--                     { start =
--                         \_ ->
--                             Hufflepuff
--                     , dwellPeriod = \_ -> Nothing
--                     , adjustor =
--                         \_ ->
--                             Timeline.linearDefault
--                     , visit =
--                         \lookup target targetTime maybeLookAhead state ->
--                             Timeline.getEvent target
--                     , lerp =
--                         \_ maybePrevious target _ _ _ state ->
--                             target
--                     }
--                     timeline

--         , benchmark "Visit" <|
--             \_ ->
--                 Interpolate.visit identity
--                     (Timeline.Occurring (Interpolate.Pos Interpolate.standardDefault 20) (Time.millis 100) (Time.millis 900))
--                     (Time.millis 500)
--                     Nothing
--                     { position = Pixels.pixels 0
--                     , velocity = Pixels.pixelsPerSecond 0
--                     }
--         ]
-- springs : Benchmark
-- springs =
--     let
--         steps =
--             List.repeat 0 20
--     in
--     describe "Springs"
--         [ benchmark "stepwise - 100 steps" <|
--             \_ ->
--                 Spring.stepOver (Animator.millis (16 * 20))
--                     { stiffness = 180
--                     , damping = 12
--                     , mass = 1
--                     }
--                     300
--                     { velocity = 0
--                     , position = 0
--                     }
--         , benchmark "Analytical measure" <|
--             \_ ->
--                 Spring.analytical
--                     { stiffness = 180
--                     , damping = 12
--                     , mass = 1
--                     }
--                     (Animator.millis (16 * 20))
--                     300
--                     { velocity = 0
--                     , position = 0
--                     }
--         , benchmark "presolved differential equation" <|
--             \_ ->
--                 let
--                     t =
--                         0.5
--                 in
--                 -- if you provide  f(0) = 0; f'(0) = 0; f''(t) = -180(f(t) - 1) - 12f'(t) as input
--                 -- this is the differential equation from wolfram alpha
--                 -- f(t) = -1/2 e^(-6 t) (-2 e^(6 t) + sin(12 t) + 2 cos(12 t))
--                 (-1 / 2 * e ^ (-6 * t)) * ((-2 * e ^ (6 * t)) + sin (12 * t) + 2 * cos (12 * t))
--         ]
-- fract x =
--     x - toFloat (floor x)
-- {-| -}
-- randomness : Benchmark
-- randomness =
--     describe "Random number generator"
--         [ benchmark "elm/random - random float" <|
--             \_ ->
--                 Random.initialSeed 8675309
--                     |> Random.step (Random.float 0 1)
--                     |> Tuple.first
--         , benchmark "scaled sine method" <|
--             -- learned this approach from https://thebookofshaders.com/10/
--             -- super cool!
--             -- and turns out 32x faster than elm/random
--             -- though elm/random has a much nicer distribution
--             \_ ->
--                 let
--                     seed =
--                         8675309
--                 in
--                 (fract (sin seed * 100000.0) + 1.5707) / 3.1415
--         ]


beforeMinus : Float -> Float -> Bool
beforeMinus one two =
    (one - two) < 0


before : Float -> Float -> Bool
before one two =
    one < two


timeOne =
    Time.absolute (Time.millisToPosix 0)


timeTwo =
    Time.absolute (Time.millisToPosix 500)


stringifying : Benchmark
stringifying =
    let
        point =
            { x = 256
            , y = 256
            }

        x =
            84923

        bez =
            Bezier.Spline point point point point
    in
    describe "Stringifying numbers"
        [ benchmark "Bezier, convert each number to string" <|
            \_ ->
                -- This now uses bitencoding, but before it was the niaeve way, which was 4 times slower.
                Bezier.hash bez
        , benchmark "Bezier, convert only one" <|
            \_ ->
                String.fromInt x
        , benchmark "Bezier, bit-encoded" <|
            \_ ->
                case bez of
                    Bezier.Spline one two three four ->
                        String.fromInt (Bits.value (Bits.store4Float one.x one.y two.x two.y))
                            ++ "-"
                            ++ String.fromInt (Bits.value (Bits.store4Float three.x three.y four.x four.y))
        ]


batman =
    "batman"


superman =
    "superman"


eightySeven =
    87


twentythree =
    23


equalityCompare : Benchmark
equalityCompare =
    describe "Comparing equality checks for primitives"
        [ benchmark "Strings equal" <|
            \_ ->
                batman == superman
        , benchmark "Ints" <|
            \_ ->
                eightySeven == twentythree
        , benchmark "Ints w/0 trick" <|
            \_ ->
                eightySeven - twentythree == 0
        ]


floatComparison : Benchmark
floatComparison =
    describe "Comparing Floats"
        [ benchmark "Standard" <|
            \_ ->
                before 5 20
        , benchmark "(one - two) < 0 form" <|
            \_ ->
                beforeMinus 5 20
        ]



-- {-| -}
-- basicInterpolation : Benchmark
-- basicInterpolation =
--     let
--         timeline =
--             Animator.init Hufflepuff
--                 |> Animator.queue
--                     [ Animator.wait (Animator.seconds 1)
--                     , Animator.transitionTo (Animator.seconds 1) Griffyndor
--                     , Animator.wait (Animator.seconds 1)
--                     , Animator.transitionTo (Animator.seconds 1) Slytherin
--                     , Animator.wait (Animator.seconds 1)
--                     , Animator.transitionTo (Animator.seconds 1) Ravenclaw
--                     , Animator.wait (Animator.seconds 1)
--                     ]
--                 |> Timeline.update (Time.millisToPosix 0)
--                 |> Timeline.update (Time.millisToPosix 1400)
--     in
--     describe "Interpolate to a point on a 4 event timeline"
--         [ benchmark "interpolate to position" <|
--             \_ ->
--                 Interpolate.details timeline
--                     (Interpolate.withStandardDefault << toPos)
--         , benchmark "capture frames(60fps)" <|
--             \_ ->
--                 Timeline.capture 60
--                     (Interpolate.withStandardDefault << toPos)
--                     Interpolate.moving
--                     timeline
--         , benchmark "capture frames(15fps)" <|
--             \_ ->
--                 Timeline.capture 15
--                     (Interpolate.withStandardDefault << toPos)
--                     Interpolate.moving
--                     timeline
--         ]
-- cssGeneration =
--     let
--         timeline =
--             Animator.init Hufflepuff
--                 |> Animator.queue
--                     [ Animator.wait (Animator.seconds 1)
--                     , Animator.transitionTo (Animator.seconds 1) Griffyndor
--                     , Animator.wait (Animator.seconds 1)
--                     , Animator.transitionTo (Animator.seconds 1) Slytherin
--                     , Animator.wait (Animator.seconds 1)
--                     , Animator.transitionTo (Animator.seconds 1) Ravenclaw
--                     , Animator.wait (Animator.seconds 1)
--                     ]
--                 |> Timeline.update (Time.millisToPosix 0)
--                 |> Timeline.update (Time.millisToPosix 1400)
--     in
--     describe "Css generation:4 event timeline"
--         [ benchmark "capture frames, 60fps, (old)" <|
--             \_ ->
--                 Timeline.capture 60
--                     (Interpolate.withStandardDefault << toPos)
--                     Interpolate.moving
--                     timeline
--         , benchmark "capture frames, 15fps, (old)" <|
--             \_ ->
--                 Timeline.capture 15
--                     (Interpolate.withStandardDefault << toPos)
--                     Interpolate.moving
--                     timeline
--         , benchmark "bezier-generation (new)" <|
--             \_ ->
--                 Css.cssFromProps
--                     timeline
--                     toProps
--         ]
-- toProps event =
--     -- let
--     --     base = toFloat state * 100
--     -- in
--     -- -- Interpolate.Pos Interpolate.standardDefault (toFloat (state * 100))
--     -- [ Css.Prop Internal.Css.Props.ids.opacity
--     --     (wave (Timeline.Repeat 5 (Animator.millis 200)) base (base + 100))
--     -- ]
--     case event of
--         Hufflepuff ->
--             [ Animator.Css2.opacity
--                 (Animator.at 100)
--             ]
--         Griffyndor ->
--             -- pos 400
--             [ Animator.Css2.opacity
--                 (Animator.at 400)
--             ]
--         Slytherin ->
--             -- pos 700
--             [ Animator.Css2.opacity
--                 (Animator.at 700)
--             ]
--         Ravenclaw ->
--             -- pos 1000
--             [ Animator.Css2.opacity
--                 (Animator.at 1000)
--             ]
-- pos state =
--     Interpolate.Pos Interpolate.standardDefault state
-- type House
--     = Hufflepuff
--     | Griffyndor
--     | Slytherin
--     | Ravenclaw
-- toPos event =
--     case event of
--         Hufflepuff ->
--             Animator.at 100
--         Griffyndor ->
--             Animator.at 400
--         Slytherin ->
--             Animator.at 700
--         Ravenclaw ->
--             Animator.at 1000
-- baseSpline =
--     Interpolate.createSpline
--         { start =
--             { x = 0
--             , y = 2
--             }
--         , startVelocity =
--             { x = 1000
--             , y = 250
--             }
--         , departure =
--             Interpolate.standardDefault
--         , end =
--             { x = 1000
--             , y = 1000
--             }
--         , endVelocity =
--             { x = 1000
--             , y = 0
--             }
--         , arrival =
--             Interpolate.standardDefault
--         }
-- interpolationComponents =
--     describe "Interpolation"
--         [ benchmark "Create spline" <|
--             \_ ->
--                 Interpolate.createSpline
--                     { start =
--                         { x = 0
--                         , y = 2
--                         }
--                     , startVelocity =
--                         { x = 1000
--                         , y = 250
--                         }
--                     , departure =
--                         Interpolate.standardDefault
--                     , end =
--                         { x = 1000
--                         , y = 1000
--                         }
--                     , endVelocity =
--                         { x = 1000
--                         , y = 0
--                         }
--                     , arrival =
--                         Interpolate.standardDefault
--                     }
--         , benchmark "Find x on spline" <|
--             \_ ->
--                 Bezier.atX baseSpline 625
--         ]
