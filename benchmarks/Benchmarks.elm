module Benchmarks exposing (main)

import Animator
import Array
import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Internal.Spring as Spring
import Random
import Time


main : BenchmarkProgram
main =
    program <|
        Benchmark.describe "Animator benchmarks"
            [ springs
            , randomness
            , basicInterpolation
            ]


springs : Benchmark
springs =
    let
        steps =
            List.repeat 0 20
    in
    describe "Spring Calculations"
        [ benchmark "stepwise - 100 steps" <|
            -- This turns out to be ~410% faster than the presolved differential equation
            \_ ->
                List.foldl
                    (\_ ->
                        Spring.step 300
                            { stiffness = 180
                            , damping = 12
                            , mass = 1
                            }
                            16
                    )
                    { velocity = 300
                    , position = 0
                    }
                    steps
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
        , benchmark "scaled sine approach" <|
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


{-| -}
basicInterpolation : Benchmark
basicInterpolation =
    let
        timeline =
            Animator.init (Time.millisToPosix 0) Hufflepuff
                |> Animator.queue
                    [ Animator.wait (Animator.seconds 1)
                    , Animator.event (Animator.seconds 1) Griffyndor
                    , Animator.wait (Animator.seconds 1)
                    , Animator.event (Animator.seconds 1) Slytherin
                    , Animator.wait (Animator.seconds 1)
                    , Animator.event (Animator.seconds 1) Ravenclaw
                    , Animator.wait (Animator.seconds 1)
                    ]
                |> Animator.update (Time.millisToPosix 0)
                |> Animator.update (Time.millisToPosix 3400)
    in
    describe "Interpolate to a point on a 4 event timeline"
        [ benchmark "interpolate to position" <|
            \_ ->
                Animator.move timeline toPos
        ]


type House
    = Hufflepuff
    | Griffyndor
    | Slytherin
    | Ravenclaw


toPos event =
    case event of
        Hufflepuff ->
            Animator.to 100

        Griffyndor ->
            Animator.to 400

        Slytherin ->
            Animator.to 700

        Ravenclaw ->
            Animator.to 1000
