module SplineCreation exposing (suite)

import Animator
import Duration
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (Fuzzer, float, int, list, string)
import Internal.Estimation as Estimate
import Internal.Interpolate as Interpolate
import Internal.Timeline as Timeline
import Pixels
import Quantity
import Test exposing (..)
import Time


suite =
    describe "Spline config"
        [ test "Standard 1 second case" <|
            \_ ->
                let
                    spline =
                        Interpolate.createSpline
                            { arrival =
                                Interpolate.standardDefault
                            , departure =
                                Interpolate.standardDefault
                            , end = { x = 2000, y = 400 }
                            , endVelocity = { x = 1000, y = 0 }
                            , start = { x = 1000, y = 100 }
                            , startVelocity = { x = 1000, y = 0 }
                            }
                in
                Expect.equal spline
                    (Interpolate.Spline
                        { x = 1000, y = 100 }
                        { x = 1400, y = 100 }
                        { x = 1200, y = 400 }
                        { x = 2000, y = 400 }
                    )
        , test "250ms second case" <|
            \_ ->
                -- There was a bug that happened because the velocity vectors were not scaled correctly
                -- which means with the test time of 1000ms, it was fine,
                -- but any other time it would be off.  This is why this test is here!
                let
                    spline =
                        Interpolate.createSpline
                            { arrival =
                                Interpolate.standardDefault
                            , departure =
                                Interpolate.standardDefault
                            , start = { x = 1000, y = 100 }
                            , startVelocity = { x = 1000, y = 0 }
                            , end = { x = 1250, y = 400 }
                            , endVelocity = { x = 1000, y = 0 }
                            }
                in
                Expect.equal spline
                    (Interpolate.Spline
                        { x = 1000, y = 100 }
                        { x = 1100, y = 100 }
                        { x = 1050, y = 400 }
                        { x = 1250, y = 400 }
                    )
        ]
