module Bezier exposing (..)

{-| -}

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float, int, list, string)
import InternalAnim.Bezier as Bezier
import InternalAnim.Duration as Duration
import InternalAnim.Move as Move
import InternalAnim.Quantity as Quantity
import InternalAnim.Time as Time
import InternalAnim.Transition as Transition
import InternalAnim.Units as Units
import Test exposing (..)


{-|

    Right now, atX is defined as


        atX :
            Float
            -> TimeDomain
            -> Units.PixelsPerSecond
            -> Units.PixelsPerSecond
            -> Transition
            ->
                { position : Units.Pixels
                , velocity : Units.PixelsPerSecond
                }


        And it

            1. converts from normalized to a new domain using inTimeDomain
                This makes it's domain to be realworld pixel coordinates

            2. AND it also adjusts for existing velocity


        However, we also want to do the velocity correction in `Move.sequences`

        Currently `Transition.keyframes` does this correction, but we always hand it an intro/exit velocity of 0.
            And there's not a convenient way of tracking intro/exit velocity for a given sequence.





    Bezier curves start out normalized between 0 and 1. Let's keep them that way!




        -- normalize transition if needed
        transition
            |> Transition.adjust
                (introVelocity
                    |> normalizeOver domain
                )
                (exitVelocity
                    |> normalizeOver domain
                )

        posNormal =
            Transition.atX2 :
                Float
                -> Transition
                ->
                    -- position -> between 0 and 1
                    { position : Float
                    -- velocity, same consideration
                    , velocity : Float
                    }

                |> Transition.toReal domain




    adjustTransition
        : NormalizedIntroVelocity
        -> NormalizedExitVelocity
        -> Transition -> Transition

-}
suite : Test
suite =
    describe "Bezier adjustments"
        [ test "Transition.atX == Move.atX at 0.5" <|
            \_ ->
                let
                    targetPosition =
                        300

                    args =
                        { progress = 0.3
                        , startPosition = 0
                        , targetPosition = targetPosition
                        , target = Move.to targetPosition
                        , startTime = Time.millis 0
                        , targetTime = Time.millis 2000
                        }

                    moveXNewState =
                        usingMoveX
                            args

                    transitionNewState =
                        usingTransitionX args
                in
                Expect.all
                    [ \state ->
                        Expect.within
                            (Absolute 1)
                            (Units.inPixels state.move.position)
                            (Units.inPixels state.transitionX.position)
                    , \state ->
                        Expect.within
                            (Absolute 1)
                            (Units.inPixelsPerSecond state.move.velocity)
                            (Units.inPixelsPerSecond state.transitionX.velocity)
                    ]
                    { move = moveXNewState
                    , transitionX = transitionNewState
                    }
        , fuzz (Fuzz.floatRange 0 1) "Transition.atX == Move.atX with Fuzz" <|
            \progress ->
                let
                    targetPosition =
                        300

                    args =
                        { progress = progress
                        , startPosition = 0
                        , targetPosition = targetPosition
                        , target = Move.to targetPosition
                        , startTime = Time.millis 0
                        , targetTime = Time.millis 2000
                        }

                    moveXNewState =
                        usingMoveX
                            args

                    transitionNewState =
                        usingTransitionX args
                in
                Expect.all
                    [ \state ->
                        Expect.within
                            (Absolute 1)
                            (Units.inPixels state.move.position)
                            (Units.inPixels state.transitionX.position)
                    , \state ->
                        Expect.within
                            (Absolute 1)
                            (Units.inPixelsPerSecond state.move.velocity)
                            (Units.inPixelsPerSecond state.transitionX.velocity)
                    ]
                    { move = moveXNewState
                    , transitionX = transitionNewState
                    }
        , test "Adjusting bezier velocities" <|
            \_ ->
                let
                    startPosition =
                        0

                    targetPosition =
                        300

                    -- taken from the above test
                    introVelocity =
                        -- pixels per second
                        410.143666198451

                    introVelNormalized =
                        (introVelocity * ((targetTime / 1000) - (startTime / 1000)))
                            / (targetPosition - startPosition)

                    startTime =
                        0

                    targetTime =
                        2000

                    args =
                        { progress = 0.001
                        , startPosition = startPosition
                        , targetPosition = targetPosition
                        , target =
                            Move.to targetPosition
                                |> Move.withVelocities
                                    introVelNormalized
                                    0
                        , startTime = Time.millis startTime
                        , targetTime = Time.millis targetTime
                        }

                    moveXNewState =
                        usingMoveX
                            args
                in
                Expect.all
                    [ \state ->
                        Expect.within
                            (Absolute 1)
                            (Units.inPixelsPerSecond state.move.velocity)
                            introVelocity
                    ]
                    { move = moveXNewState
                    }
        ]


usingMoveX :
    { progress : Float
    , target : Move.Move Float
    , startPosition : Float
    , targetPosition : Float
    , startTime : Time.Absolute
    , targetTime : Time.Absolute
    }
    -> Move.State
usingMoveX args =
    let
        newNormalizedState =
            Move.atX
                args.progress
                args.target
    in
    { position =
        Units.pixels
            (Move.toReal
                args.startPosition
                args.targetPosition
                newNormalizedState.position.y
            )
    , velocity =
        let
            scaled =
                newNormalizedState.velocity
                    |> Bezier.scaleXYBy
                        { x =
                            Duration.inSeconds
                                (Time.duration args.startTime args.targetTime)
                        , y = args.targetPosition - args.startPosition
                        }
        in
        Units.pixelsPerSecond (scaled.y / scaled.x)
    }


usingTransitionX :
    { progress : Float
    , target : Move.Move Float
    , startPosition : Float
    , targetPosition : Float
    , startTime : Time.Absolute
    , targetTime : Time.Absolute
    }
    -> Move.State
usingTransitionX args =
    let
        startingVelocity =
            Units.pixelsPerSecond 0

        targetVelocity =
            --Estimation.velocityAtTarget lookupState target future
            Units.pixelsPerSecond 0

        targetTransition =
            case args.target of
                Move.Pos trans _ _ ->
                    trans

        domain =
            { start =
                { x = args.startTime
                , y = Units.pixels args.startPosition
                }
            , end =
                { x = args.targetTime
                , y = Units.pixels args.targetPosition
                }
            }
    in
    Transition.atX
        args.progress
        domain
        startingVelocity
        targetVelocity
        targetTransition
