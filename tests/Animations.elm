module Animations exposing (suite)

import Animator
import Animator.Timeline as Timeline
import Expect
import Support.Css as Css
import Test exposing (Test, describe, test)
import Time


suite : Test
suite =
    describe "CSS transitions and timeline animations"
        [ test "A standalone transition uses CSS transition and sets its destination" <|
            \_ ->
                Animator.transition (Animator.ms 1000) [ Animator.opacity 0.5 ]
                    |> Animator.toCss
                    |> Expect.all
                        [ \css -> Expect.equal (Just "0.5") (Css.property "opacity" css)
                        , \css -> Expect.notEqual Nothing (Css.property "transition" css)
                        , \css -> Expect.equal "" css.keyframes
                        , \css -> Expect.equal Nothing (Css.property "animation" css)
                        ]
        , test "A scheduled transition uses keyframes so its timing is controlled by the timeline" <|
            \_ ->
                Animator.css timeline (\opacity -> ( [ Animator.opacity opacity ], [] ))
                    |> Expect.all
                        [ \css -> Expect.notEqual Nothing (Css.property "animation" css)
                        , \css -> Expect.equal Nothing (Css.property "transition" css)
                        , \css ->
                            case Css.keyframes css.keyframes of
                                Err error ->
                                    Expect.fail error

                                Ok blocks ->
                                    Expect.equal 1 (List.length blocks)
                        ]
        , test "Advancing the clock without rescheduling does not restart CSS animation" <|
            \_ ->
                let
                    render tl =
                        Animator.css tl (\opacity -> ( [ Animator.opacity opacity ], [] ))
                in
                Expect.equal
                    (render timeline)
                    (render (Timeline.update (Time.millisToPosix 501) timeline))
        ]


timeline : Timeline.Timeline Float
timeline =
    Timeline.init 1
        |> Timeline.to (Animator.ms 1000) 0.5
        |> Timeline.update (Time.millisToPosix 1)
