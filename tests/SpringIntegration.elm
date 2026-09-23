module SpringIntegration exposing (suite)

import Animator as Anim
import Animator.Timeline as Timeline
import Animator.Transition
import Bezier
import Bezier.Spring as Spring
import Dict
import Expect
import InternalAnim.Time as InternalTime
import InternalAnim.Transition as Transition
import InternalAnim.Units as Units
import Support.Css as Css
import Test exposing (Test, describe, test)
import Time


suite : Test
suite =
    describe "elm-bezier 2 integration"
        (List.map segmentDomain [ -120, 0, 0.5, 100, 2000 ]
            ++ [ test "The spring evaluator retains its initial position and units-per-second velocity" <|
                    \_ ->
                        Spring.at
                            { spring = parameters
                            , initial = { position = 17, velocity = 137 }
                            , target = 100
                            }
                            0
                            |> Expect.all
                                [ .position >> Expect.within (Expect.Absolute 0.000001) 17
                                , .velocity >> Expect.within (Expect.Absolute 0.000001) 137
                                ]
               , test "Animator preserves incoming spring velocity at zero progress" <|
                    \_ ->
                        sample 0 (Transition.wobble { wobble = 1, quickness = 0 })
                            |> .velocity
                            |> Units.inPixelsPerSecond
                            |> Expect.within (Expect.Absolute 0.000001) 137
               , test "An explicit intro velocity applies consistently throughout the spring" <|
                    \_ ->
                        let
                            actual =
                                Transition.wobble { wobble = 1, quickness = 0 }
                                    |> Transition.withVelocities -80 0
                                    |> sample 0.1

                            expected =
                                Spring.at
                                    { spring = parameters
                                    , initial = { position = 17, velocity = -80 }
                                    , target = 100
                                    }
                                    100
                        in
                        Expect.all
                            [ \_ -> Expect.within (Expect.Absolute 0.000001) expected.position (Units.inPixels actual.position)
                            , \_ -> Expect.within (Expect.Absolute 0.000001) expected.velocity (Units.inPixelsPerSecond actual.velocity)
                            ]
                            ()
               , test "A spring interruption at its current position still carries momentum" <|
                    \_ ->
                        let
                            css =
                                Timeline.init 0
                                    |> Timeline.to (Anim.ms 1000) 200
                                    |> Timeline.update (Time.millisToPosix 10000)
                                    |> Timeline.update (Time.millisToPosix 10500)
                                    |> Timeline.to (Anim.ms 1000) 100
                                    |> Timeline.update (Time.millisToPosix 10500)
                                    |> (\timeline ->
                                            Anim.onTimeline timeline
                                                (\value ->
                                                    [ Anim.float "left" value
                                                        |> Anim.withTransition
                                                            (if value == 200 then
                                                                Animator.Transition.linear

                                                             else
                                                                Animator.Transition.spring { wobble = 1, quickness = 0 }
                                                            )
                                                    ]
                                                )
                                       )
                                    |> Anim.toCss
                        in
                        case Css.keyframes css.keyframes of
                            Ok [ block ] ->
                                let
                                    positions =
                                        List.filterMap (.declarations >> Dict.get "left" >> Maybe.andThen String.toFloat) block.frames
                                in
                                Expect.all
                                    [ \_ -> Expect.equal (Just 100) (List.head positions)
                                    , \_ -> Expect.greaterThan 100 (List.maximum positions |> Maybe.withDefault 0)
                                    , \_ -> Expect.equal (Just 100) (List.reverse positions |> List.head)
                                    ]
                                    ()

                            _ ->
                                Expect.fail "Expected one replacement spring animation"
               ]
        )


parameters : Spring.Parameters
parameters =
    Spring.new { wobble = 1, quickness = 0, settleMax = 1000 }


segmentDomain : Float -> Test
segmentDomain target =
    test ("Segment times span the settling time, independent of target " ++ String.fromFloat target) <|
        \_ ->
            let
                segments =
                    Spring.segments parameters { position = 17, velocity = 137 } target
            in
            case ( List.head segments, List.reverse segments |> List.head ) of
                ( Just first, Just last ) ->
                    Expect.all
                        [ \_ -> Expect.within (Expect.Absolute 0.000001) 0 (Bezier.first first).x
                        , \_ -> Expect.within (Expect.Absolute 0.000001) 17 (Bezier.first first).y
                        , \_ -> Expect.within (Expect.Absolute 0.000001) (Spring.settlesAt parameters) (Bezier.last last).x
                        , \_ -> Expect.equal True (List.all (\segment -> (Bezier.last segment).x > (Bezier.first segment).x) segments)
                        ]
                        ()

                _ ->
                    Expect.fail "Expected spring segments"


sample : Float -> Transition.Transition -> { position : Units.Pixels, velocity : Units.PixelsPerSecond }
sample progress curve =
    Transition.atX progress
        (InternalTime.millis 0)
        (InternalTime.millis 1000)
        curve
        { position = Units.pixels 17, velocity = Units.pixelsPerSecond 137 }
        100
