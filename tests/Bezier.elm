module Bezier exposing (..)

{-| -}

import Duration
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float, int, list, string)
import Internal.Bezier as Bezier
import Internal.Move as Move
import Internal.Random
import Internal.Time as Time
import Internal.Transition as Transition
import Internal.Units as Units
import Pixels
import Quantity
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
                            (Pixels.inPixels state.move.position)
                            (Pixels.inPixels state.transitionX.position)
                    , \state ->
                        Expect.within
                            (Absolute 1)
                            (Debug.log "VELOCITY" (Pixels.inPixelsPerSecond state.move.velocity))
                            (Pixels.inPixelsPerSecond state.transitionX.velocity)
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
                            (Pixels.inPixels state.move.position)
                            (Pixels.inPixels state.transitionX.position)
                    , \state ->
                        Expect.within
                            (Absolute 1)
                            (Pixels.inPixelsPerSecond state.move.velocity)
                            (Pixels.inPixelsPerSecond state.transitionX.velocity)
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
                            (Pixels.inPixelsPerSecond state.move.velocity)
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
    ->
        { position : Quantity.Quantity Float Pixels.Pixels
        , velocity : Quantity.Quantity Float Pixels.PixelsPerSecond
        }
usingMoveX args =
    let
        newNormalizedState =
            Move.atX
                args.progress
                args.target
    in
    { position =
        Pixels.pixels
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
        Pixels.pixelsPerSecond (scaled.y / scaled.x)
    }


usingTransitionX :
    { progress : Float
    , target : Move.Move Float
    , startPosition : Float
    , targetPosition : Float
    , startTime : Time.Absolute
    , targetTime : Time.Absolute
    }
    ->
        { position : Quantity.Quantity Float Pixels.Pixels
        , velocity : Quantity.Quantity Float Pixels.PixelsPerSecond
        }
usingTransitionX args =
    let
        startingVelocity =
            Pixels.pixelsPerSecond 0

        targetVelocity =
            --Interpolate.velocityAtTarget lookupState target future
            Pixels.pixelsPerSecond 0

        targetTransition =
            case args.target of
                Move.Pos trans _ _ ->
                    trans

        domain =
            { start =
                { x = args.startTime
                , y = Pixels.pixels args.startPosition
                }
            , end =
                { x = args.targetTime
                , y = Pixels.pixels args.targetPosition
                }
            }
    in
    Transition.atX
        args.progress
        domain
        startingVelocity
        targetVelocity
        targetTransition
