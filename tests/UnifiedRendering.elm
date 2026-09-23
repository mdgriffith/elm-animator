module UnifiedRendering exposing (suite)

import Animator as Anim
import Animator.Timeline as Timeline
import Animator.Transition as Transition
import Color
import Dict
import Expect
import Support.Css as Css
import Test exposing (Test, describe, test)
import Time


suite : Test
suite =
    describe "Unified renderer"
        [ test "Explicit transitions support grouped transforms, colors, and scalar properties" <|
            \_ ->
                Anim.transition (Anim.ms 300)
                    [ Anim.x 12, Anim.y 34, Anim.scale 2, Anim.color "color" Color.red, Anim.opacity 0.5 ]
                    |> Anim.toCss
                    |> Expect.all
                        [ \css -> Expect.equal "" css.keyframes
                        , \css -> Expect.equal Nothing (Css.property "animation" css)
                        , \css -> Expect.equal (Just "12px 34px 0px") (Css.property "translate" css)
                        , \css -> Expect.equal (Just "2 2 2") (Css.property "scale" css)
                        , \css -> Expect.equal (Just (Color.toCssString Color.red)) (Css.property "color" css)
                        , \css -> Expect.equal True (List.all (\name -> String.contains (name ++ " 300ms") css.transition) [ "translate", "scale", "color", "opacity" ])
                        ]
        , test "An explicit spring transition falls back to bounded keyframes" <|
            \_ ->
                Anim.transition (Anim.ms 1000)
                    [ Anim.float "left" 100 |> Anim.withTransition (Transition.spring { wobble = 1, quickness = 0 }) ]
                    |> Anim.toCss
                    |> Expect.all
                        [ \css -> Expect.equal "" css.transition
                        , \css -> Expect.notEqual Nothing (Css.property "animation" css)
                        , \css ->
                            case Css.keyframes css.keyframes of
                                Ok [ block ] ->
                                    let
                                        values =
                                            List.filterMap (.declarations >> Dict.get "left" >> Maybe.andThen String.toFloat) block.frames
                                    in
                                    Expect.all
                                        [ \_ -> Expect.equal (List.length block.frames) (List.length values)
                                        , \_ -> Expect.equal (Just 0) (List.head values)
                                        , \_ -> Expect.equal (Just 100) (List.reverse values |> List.head)
                                        , \_ -> Expect.greaterThan 100 (List.maximum values |> Maybe.withDefault 0)
                                        , \_ -> Expect.atLeast -100 (List.minimum values |> Maybe.withDefault 0)
                                        , \_ -> Expect.atMost 200 (List.maximum values |> Maybe.withDefault 0)
                                        ]
                                        ()

                                _ ->
                                    Expect.fail "Expected one spring animation"
                        ]
        , test "Zero-duration spring transitions immediately set the target" <|
            \_ ->
                Anim.transition (Anim.ms 0)
                    [ Anim.opacity 0.3 |> Anim.withTransition (Transition.spring { wobble = 1, quickness = 0 }) ]
                    |> Anim.toCss
                    |> Expect.all
                        [ \css -> Expect.equal (Just "0.3") (Css.property "opacity" css)
                        , \css -> Expect.equal "" css.keyframes
                        ]
        , test "Zero-duration keyframes set the final value without invalid percentages" <|
            \_ ->
                Anim.keyframes [ Anim.set [ Anim.opacity 0 ], Anim.step (Anim.ms 0) [ Anim.opacity 0.7 ] ]
                    |> Anim.toCss
                    |> Expect.all
                        [ \css -> Expect.equal (Just "0.7") (Css.property "opacity" css)
                        , \css -> Expect.equal "" css.keyframes
                        ]
        , test "A repeat count of zero does not introduce properties" <|
            \_ ->
                Anim.keyframes [ Anim.loopFor 0 [ Anim.set [ Anim.opacity 0.7 ] ] ]
                    |> Anim.toCss
                    |> .props
                    |> Expect.equal []
        , test "Empty infinite loops do not swallow subsequent steps" <|
            \_ ->
                Anim.keyframes
                    [ Anim.sequence [ Anim.loop [] ]
                    , Anim.set [ Anim.opacity 0.7 ]
                    ]
                    |> Anim.toCss
                    |> Css.property "opacity"
                    |> Expect.equal (Just "0.7")
        , test "Fractional millisecond durations survive CSS encoding" <|
            \_ ->
                Anim.keyframes [ Anim.set [ Anim.opacity 0 ], Anim.step (Anim.ms 0.25) [ Anim.opacity 1 ] ]
                    |> Anim.toCss
                    |> Css.property "animation"
                    |> Maybe.map (String.startsWith "0.25ms ")
                    |> Expect.equal (Just True)
        , test "Identical targets with different starting values do not collide in the stylesheet" <|
            \_ ->
                let
                    render start =
                        Timeline.init start
                            |> Timeline.to (Anim.ms 1000) 1
                            |> Timeline.update (Time.millisToPosix 10000)
                            |> (\tl -> Anim.onTimeline tl (\value -> [ Anim.opacity value ]))
                            |> Anim.toCss
                in
                Expect.notEqual (render 0).keyframes (render 0.5).keyframes
        , test "CSS identity includes duration and repeat count" <|
            \_ ->
                let
                    render count duration =
                        Anim.keyframes
                            [ Anim.loopFor count
                                [ Anim.set [ Anim.opacity 0 ]
                                , Anim.step (Anim.ms duration) [ Anim.opacity 1 ]
                                ]
                            ]
                            |> Anim.toCss
                in
                Expect.all
                    [ \_ -> Expect.notEqual (render 2 1000).hash (render 2 2000).hash
                    , \_ -> Expect.notEqual (render 2 1000).hash (render 3 1000).hash
                    ]
                    ()
        , test "A large timeline renders without overflowing the stack" <|
            \_ ->
                Timeline.init 0
                    |> Timeline.queue (List.map (Timeline.transitionTo (Anim.ms 1)) (List.range 1 10000))
                    |> Timeline.update (Time.millisToPosix 10000)
                    |> (\tl -> Anim.onTimeline tl (\value -> [ Anim.float "left" (toFloat value) ]))
                    |> Anim.toCss
                    |> Css.property "animation"
                    |> Maybe.map (String.split "," >> List.length)
                    |> Expect.equal (Just 10000)
        , test "An interrupted CSS timeline starts from its sampled position" <|
            \_ ->
                let
                    css =
                        Timeline.init 0
                            |> Timeline.to (Anim.ms 1000) 1000
                            |> Timeline.update (Time.millisToPosix 10000)
                            |> Timeline.update (Time.millisToPosix 10500)
                            |> Timeline.to (Anim.ms 1000) 2000
                            |> Timeline.update (Time.millisToPosix 10500)
                            |> (\tl -> Anim.onTimeline tl (\value -> [ Anim.float "left" value |> Anim.withTransition Transition.linear ]))
                            |> Anim.toCss
                in
                case Css.keyframes css.keyframes of
                    Ok [ block ] ->
                        Expect.equal
                            [ ( 0, Just "500" ), ( 100, Just "2000" ) ]
                            (List.map (\frame -> ( frame.percent, Dict.get "left" frame.declarations )) block.frames)

                    _ ->
                        Expect.fail "Expected only the replacement animation"
        ]
