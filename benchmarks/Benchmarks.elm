module Benchmarks exposing (main)

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


main : BenchmarkProgram
main =
    program <|
        Benchmark.describe "Animator benchmarks"
            [ springs
            , randomness
            , basicInterpolation
            , unwrappingValues
            , floatComparison
            , interpolationComponents
            , functionCalling
            ]


springs : Benchmark
springs =
    let
        steps =
            List.repeat 0 20
    in
    describe "Spring Calculations"
        [ benchmark "stepwise - 100 steps" <|
            -- This turns out to be ~410% faster than the presolved differential equation on FF
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
        [ benchmark "Normal float compare" <|
            \_ ->
                before 5 20
        , benchmark "Minus trick, float compare" <|
            \_ ->
                beforeMinus 5 20
        , benchmark "Unwrapping and comparison" <|
            \_ ->
                Time.thisBeforeThat timeOne timeTwo
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
                Animator.details timeline toPos
        , benchmark "iterating generation(60fps)" <|
            \_ ->
                Timeline.capture 60
                    toPos
                    Interpolate.moving
                    timeline
        , benchmark "iterating generation(15fps), but interpolated" <|
            \_ ->
                Timeline.capture 15
                    toPos
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


functionWithRecord { one, two, three, four, five, six } =
    one * two * three


functionWithArgs one two three four five six =
    one * two * three


functionCalling =
    let
        one =
            5

        two =
            10

        three =
            1000

        four =
            "string"

        five =
            True

        six =
            Just 52
    in
    describe "Is having arguments as a record significantly different from positional?"
        -- Normally we shouldnt consider this, but for lib internals, why not?
        -- records are twice as fast!
        -- my guess is because they skip the wrapping of functions that elm does for currying.
        [ benchmark "Function with record arg" <|
            \_ ->
                functionWithRecord
                    { one = one
                    , two = two
                    , three = three
                    , four = four
                    , five = five
                    , six = six
                    }
        , benchmark "Function with positional" <|
            \_ ->
                functionWithArgs one two three four five six
        ]


type Wrapped
    = Wrapped Int Int Int


wrapped =
    Wrapped 1 2 3


getY (Wrapped x y z) =
    y


record =
    WrappedRecord 1 2 3


type alias WrappedRecord =
    { x : Int
    , y : Int
    , z : Int
    }


unwrappingValues =
    describe "Is having records in a type faster than a record?"
        -- Normally we shouldnt consider this, but for lib internals, why not?
        -- records are twice as fast!
        -- my guess is because they skip the wrapping of functions that elm does for currying.
        [ benchmark "Wrapped in type" <|
            \_ ->
                case wrapped of
                    Wrapped x y z ->
                        y
        , benchmark "Wrapped in type, accessor fn" <|
            \_ ->
                getY wrapped
        , benchmark "Wrapped in record" <|
            \_ ->
                record.y
        ]


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
            Interpolate.defaultDeparture
        , end =
            { x = 1000
            , y = 1000
            }
        , endVelocity =
            { x = 1000
            , y = 0
            }
        , arrival =
            Interpolate.defaultArrival
        }


interpolationComponents =
    describe "How fast are the differnt steps of interpolation"
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
                        Interpolate.defaultDeparture
                    , end =
                        { x = 1000
                        , y = 1000
                        }
                    , endVelocity =
                        { x = 1000
                        , y = 0
                        }
                    , arrival =
                        Interpolate.defaultArrival
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
