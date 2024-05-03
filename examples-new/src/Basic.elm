module Basic exposing (main)

{-| -}

import Animator
import Animator.Transition as Transition
import Color
import Html exposing (Html)
import Html.Attributes as Attr


main =
    Html.div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "gap" "40px"
        , Attr.style "width" "100vw"
        , Attr.style "height" "100vh"
        , Attr.style "justify-content" "center"
        , Attr.style "align-items" "center"
        ]
        [ stylesheet
        , title "Some built-in animations"
        , Html.div [ Attr.style "display" "flex", Attr.style "gap" "40px" ]
            [ box (Animator.spinning (Animator.ms 500))
            , box (Animator.pulsing (Animator.ms 2000))
            , box (Animator.pinging (Animator.ms 1000))
            , box (Animator.bouncing (Animator.ms 1000) 40)
            ]
        , title "Animating with springs"
        , Html.div [ Attr.style "display" "flex", Attr.style "gap" "40px" ]
            [ box (spinning { wobble = 0, duration = Animator.ms 1000 })
            , box (spinning { wobble = 0.5, duration = Animator.ms 1000 })
            , box (spinning { wobble = 1, duration = Animator.ms 1000 })
            ]
        ]


title : String -> Html msg
title text =
    Html.h1
        [ Attr.style "font-size" "2em"
        , Attr.style "font-weight" "bold"
        ]
        [ Html.text text ]


spinning : { wobble : Float, duration : Animator.Duration } -> Animator.Animation
spinning { wobble, duration } =
    Animator.keyframes
        [ Animator.loop
            [ Animator.set
                [ Animator.rotation 0
                ]
            , Animator.step duration
                [ Animator.rotation 0.5
                    |> Animator.withTransition
                        (Transition.spring
                            { wobble = wobble
                            , quickness = 0
                            }
                        )
                ]
            ]
        ]


stylesheet : Html msg
stylesheet =
    Html.node "style"
        []
        [ Html.text """

body {
    width: 100vw;
    height: 100vh;
    font-family: "EB Garamond", serif;
}


"""
        ]


box animation =
    Animator.div animation
        [ Attr.class "box"
        , Attr.style "box-sizing" "border-box"
        , Attr.style "position" "relative"
        , Attr.style "width" "100px"
        , Attr.style "height" "100px"
        , Attr.style "border-radius" "20%"
        , Attr.style "background-color" "red"
        , Attr.style "border-color" "black"
        , Attr.style "border-style" "solid"
        ]
        []
