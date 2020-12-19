module Benchmarks exposing (suite)

import Animator
import Animator.Css
import Array
import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Internal.Interpolate as Interpolate
import Internal.Spring as Spring
import Internal.Time as Time
import Internal.Timeline as Timeline
import Pixels
import Random
import Time


suite =
    Benchmark.describe "Animator benchmarks"
        [ 
            
        --     springs
        -- , randomness
        -- , basicInterpolation
        -- , floatComparison
        -- , interpolationComponents
        foldp
        ]


foldp =
    let
        timeline =
            Animator.init Hufflepuff
                |> Animator.queue
                    [ Animator.wait (Animator.seconds 1)
                    , Animator.event (Animator.seconds 1) Griffyndor
                    , Animator.wait (Animator.seconds 1)
                    , Animator.event (Animator.seconds 1) Slytherin
                    , Animator.wait (Animator.seconds 1)
                    , Animator.event (Animator.seconds 1) Ravenclaw
                    , Animator.wait (Animator.seconds 1)
                    ]
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 1400)
    in
    describe "foldp"
        [ benchmark "new foldp" <| 
            \_ -> 
                Timeline.foldp 
                    identity 
                    { start =
                        \_ ->
                            Hufflepuff
                    , dwellPeriod = \_ -> Nothing
                    , adjustor =
                        \_ ->
                            Timeline.linearDefault
                    , visit =
                        \lookup target targetTime maybeLookAhead state ->
                            Timeline.getEvent target
                    , lerp =
                        \_ maybePrevious target _ _ _ state ->
                            target
                    }

                    timeline

        , benchmark "old foldp" <| 
            \_ -> 
                Timeline.foldpOld
                    identity 
                    { start =
                        \_ ->
                            Hufflepuff
                    , dwellPeriod = \_ -> Nothing
                    , adjustor =
                        \_ ->
                            Timeline.linearDefault
                    , visit =
                        \lookup target targetTime maybeLookAhead state ->
                            Timeline.getEvent target
                    , lerp =
                        \_ maybePrevious target _ _ _ state ->
                            target
                    }
                    timeline


        ]


springs : Benchmark
springs =
    let
        steps =
            List.repeat 0 20
    in
    describe "Springs"
        [ benchmark "stepwise - 100 steps" <|
            \_ ->
                Spring.stepOver  (Animator.millis (16 * 20))
                    { stiffness = 180
                    , damping = 12
                    , mass = 1
                    } 
                    300
                    { velocity = 0
                    , position = 0
                    }
        , benchmark "Analytical measure" <|
            \_ ->
                Spring.analytical 
                    { stiffness = 180
                    , damping = 12
                    , mass = 1
                    }
                    (Animator.millis (16 * 20))
                    300
                    { velocity = 0
                    , position = 0
                    }
        , benchmark "presolved differential equation" <|
            \_ ->
                let
                    t =
                        0.5
                in
                -- if you provide  f(0) = 0; f'(0) = 0; f''(t) = -180(f(t) - 1) - 12f'(t) as input
                -- this is the differential equation from wolfram alpha
                -- f(t) = -1/2 e^(-6 t) (-2 e^(6 t) + sin(12 t) + 2 cos(12 t))
                (-1 / 2 * e ^ (-6 * t)) * ((-2 * e ^ (6 * t)) + sin (12 * t) + 2 * cos (12 * t))
        ]


fract x =
    x - toFloat (floor x)


{-| -}
randomness : Benchmark
randomness =
    describe "Random number generator"
        [ benchmark "elm/random - random float" <|
            \_ ->
                Random.initialSeed 8675309
                    |> Random.step (Random.float 0 1)
                    |> Tuple.first
        , benchmark "scaled sine method" <|
            -- learned this approach from https://thebookofshaders.com/10/
            -- super cool!
            -- and turns out 32x faster than elm/random
            -- though elm/random has a much nicer distribution
            \_ ->
                let
                    seed =
                        8675309
                in
                (fract (sin seed * 100000.0) + 1.5707) / 3.1415
        ]


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


{-| -}
basicInterpolation : Benchmark
basicInterpolation =
    let
        timeline =
            Animator.init Hufflepuff
                |> Animator.queue
                    [ Animator.wait (Animator.seconds 1)
                    , Animator.event (Animator.seconds 1) Griffyndor
                    , Animator.wait (Animator.seconds 1)
                    , Animator.event (Animator.seconds 1) Slytherin
                    , Animator.wait (Animator.seconds 1)
                    , Animator.event (Animator.seconds 1) Ravenclaw
                    , Animator.wait (Animator.seconds 1)
                    ]
                |> Timeline.update (Time.millisToPosix 0)
                |> Timeline.update (Time.millisToPosix 1400)
    in
    describe "Interpolate to a point on a 4 event timeline"
        [ benchmark "interpolate to position" <|
            \_ ->
                Interpolate.details timeline
                    (Interpolate.withStandardDefault << toPos)
        , benchmark "capture frames(60fps)" <|
            \_ ->
                Timeline.capture 60
                    (Interpolate.withStandardDefault << toPos)
                    Interpolate.moving
                    timeline
        , benchmark "capture frames(15fps)" <|
            \_ ->
                Timeline.capture 15
                    (Interpolate.withStandardDefault << toPos)
                    Interpolate.moving
                    timeline
        ]


type House
    = Hufflepuff
    | Griffyndor
    | Slytherin
    | Ravenclaw


toPos event =
    case event of
        Hufflepuff ->
            Animator.at 100

        Griffyndor ->
            Animator.at 400

        Slytherin ->
            Animator.at 700

        Ravenclaw ->
            Animator.at 1000


baseSpline =
    Interpolate.createSpline
        { start =
            { x = 0
            , y = 2
            }
        , startVelocity =
            { x = 1000
            , y = 250
            }
        , departure =
            Interpolate.standardDefault
        , end =
            { x = 1000
            , y = 1000
            }
        , endVelocity =
            { x = 1000
            , y = 0
            }
        , arrival =
            Interpolate.standardDefault
        }


interpolationComponents =
    describe "Interpolation"
        [ benchmark "Create spline" <|
            \_ ->
                Interpolate.createSpline
                    { start =
                        { x = 0
                        , y = 2
                        }
                    , startVelocity =
                        { x = 1000
                        , y = 250
                        }
                    , departure =
                        Interpolate.standardDefault
                    , end =
                        { x = 1000
                        , y = 1000
                        }
                    , endVelocity =
                        { x = 1000
                        , y = 0
                        }
                    , arrival =
                        Interpolate.standardDefault
                    }
        , benchmark "Find x on spline" <|
            \_ ->
                Interpolate.findAtXOnSpline baseSpline
                    625
                    -- tolerance
                    1
                    -- jumpSize
                    0.25
                    -- starting t
                    0.5
                    -- depth
                    0
        ]
