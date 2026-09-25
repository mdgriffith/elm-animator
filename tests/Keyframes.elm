module Keyframes exposing (suite)

import Dict
import Expect
import InternalAnim.Transition as Transition
import Support.Css as Css
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Transition keyframe semantics"
        [ test "Standard transition specifies both endpoints and easing on the starting frame" <|
            \_ ->
                framesFor Transition.standard
                    |> expectFrames
                        (\frames ->
                            Expect.equal
                                [ { percent = 0
                                  , declarations = Dict.fromList [ ( "opacity", "0" ), ( "animation-timing-function", "cubic-bezier(0.4,0,0.2,1)" ) ]
                                  }
                                , { percent = 100, declarations = Dict.singleton "opacity" "1" }
                                ]
                                frames
                        )
        , test "Spring samples stay in the value domain, including the final frame" <|
            \_ ->
                framesFor (Transition.wobble { wobble = 1, quickness = 0 })
                    |> expectFrames
                        (\frames ->
                            let
                                values =
                                    List.filterMap (.declarations >> Dict.get "opacity" >> Maybe.andThen String.toFloat) frames
                            in
                            Expect.all
                                [ \_ -> Expect.greaterThan 2 (List.length frames)
                                , \_ -> Expect.equal (Just 100) (List.reverse frames |> List.head |> Maybe.map .percent)
                                , \_ -> Expect.equal True (List.all (\frame -> frame.percent >= 0 && frame.percent <= 100) frames)
                                , \_ -> Expect.equal (List.length frames) (List.length values)
                                , \_ -> Expect.equal (Just 0) (List.head values)
                                , \_ ->
                                    case List.reverse values |> List.head of
                                        Just final ->
                                            Expect.within (Expect.Absolute 0.02) 1 final

                                        Nothing ->
                                            Expect.fail "Missing final spring frame"
                                , \_ -> Expect.equal True (List.all (\value -> value >= -1 && value <= 2) values)
                                , \_ -> Expect.equal True (List.any ((<) 1) values)
                                ]
                                ()
                        )
        , test "A zero-length percentage interval emits no frames" <|
            \_ ->
                Transition.keyframes String.fromFloat 50 50 Transition.standard
                    |> Expect.equal ""
        , test "Spring segment times map into the requested percentage interval" <|
            \_ ->
                ("@keyframes test {"
                    ++ Transition.keyframes (\value -> "opacity:" ++ String.fromFloat value) 20 80 (Transition.wobble { wobble = 1, quickness = 0 })
                    ++ "}"
                )
                    |> Css.keyframes
                    |> expectFrames
                        (\frames ->
                            Expect.all
                                [ \_ -> Expect.equal (Just 20) (List.head frames |> Maybe.map .percent)
                                , \_ -> Expect.equal (Just 80) (List.reverse frames |> List.head |> Maybe.map .percent)
                                , \_ -> Expect.equal True (List.all (\frame -> frame.percent >= 20 && frame.percent <= 80) frames)
                                ]
                                ()
                        )
        ]


framesFor : Transition.Transition -> Result String (List Css.Keyframes)
framesFor transition =
    "@keyframes test {"
        ++ Transition.keyframes (\value -> "opacity:" ++ String.fromFloat value) 0 100 transition
        ++ "}"
        |> Css.keyframes


expectFrames : (List Css.Frame -> Expect.Expectation) -> Result String (List Css.Keyframes) -> Expect.Expectation
expectFrames check result =
    case result of
        Ok [ block ] ->
            check block.frames

        Ok _ ->
            Expect.fail "Expected exactly one keyframes block"

        Err error ->
            Expect.fail error
