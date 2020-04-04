module Checkbox exposing (main)

{-| -}

import Animator
import Animator.Inline
import Browser
import Color
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Internal.Timeline
import Time


main =
    Browser.document
        { init =
            \() ->
                ( { transitions = Animator.init False
                  , spin = Animator.init False
                  , checked = Animator.init False
                  , page = Animator.init Checkboxes
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


{-| -}
subscriptions model =
    Animator.animator Tick
        -- we tell the animator how to get the checked timeline using .checked
        -- and we tell the animator how to update that timeline with updateChecked
        |> Animator.with .checked updateChecked
        |> Animator.with .spin updateSpin
        |> Animator.with .transitions updateTransitions
        |> Animator.with .page updatePage
        |> Animator.toSubscription model


updateChecked newChecked model =
    { model | checked = newChecked }


updateSpin newSpin model =
    { model | spin = newSpin }


updateTransitions newTransitions model =
    { model | transitions = newTransitions }


updatePage newPage model =
    { model | page = newPage }


{--}
type alias Model =
    { transitions : Animator.Timeline Bool
    , spin : Animator.Timeline Bool
    , checked : Animator.Timeline Bool
    , page : Animator.Timeline Page
    }


type Page
    = Checkboxes
    | AboutPageTransitions
    | ThatsIt


next page =
    case page of
        Checkboxes ->
            AboutPageTransitions

        AboutPageTransitions ->
            ThatsIt

        ThatsIt ->
            Checkboxes


type Msg
    = Tick Model
    | Check Bool
    | NextPage
    | Spin Bool
    | Transitions Bool


update msg model =
    case msg of
        Tick newModel ->
            ( newModel
            , Cmd.none
            )

        Check bool ->
            let
                duration =
                    if Animator.current model.transitions then
                        Animator.millis 2000

                    else
                        Animator.millis 0
            in
            ( { model
                | checked =
                    Animator.interrupt
                        [ Animator.event duration bool
                        ]
                        model.checked
              }
            , Cmd.none
            )

        NextPage ->
            ( { model
                | page =
                    Animator.interrupt
                        [ Animator.event (Animator.millis 500) (next (Animator.current model.page))
                        ]
                        model.page
              }
            , Cmd.none
            )

        Spin bool ->
            ( { model
                | spin =
                    Animator.interrupt
                        [ Animator.event (Animator.millis 200) bool
                        ]
                        model.spin
              }
            , Cmd.none
            )

        Transitions bool ->
            ( { model
                | transitions =
                    Animator.interrupt
                        [ Animator.event (Animator.millis 200) bool
                        ]
                        model.transitions
              }
            , Cmd.none
            )


view model =
    { title = "Animator - Checkbox"
    , body =
        [ Html.node "style"
            []
            [ text """@import url('https://fonts.googleapis.com/css?family=Roboto&display=swap');"""
            ]
        , div
            [ Attr.style "width" "100%"
            , Attr.style "height" "1000px"
            , Attr.style "font-size" "48px"
            , Attr.style "user-select" "none"
            , Attr.style "padding" "50px"
            , Attr.style "font-family" "'Roboto', sans-serif"
            ]
            [ div
                [ Attr.style "display" "flex"
                , Attr.style "width" "100%"
                , Attr.style "align-items" "center"
                , Attr.style "justify-content" "center"
                , Attr.style "margin-bottom" "48px"
                ]
                [ span [ Attr.style "margin-right" "32px" ] [ viewCheckbox "Transitions" model.transitions Transitions ]
                , viewCheckbox "Spin" model.spin Spin
                , span
                    [ Events.onClick NextPage
                    , Attr.style "transform" "rotate(90deg)"
                    , Attr.style "margin-left" "32px"
                    , Attr.style "color" "#CCC"
                    ]
                    [ text "▲"
                    ]
                ]
            , viewPage model
            ]
        ]
    }


viewPage model =
    div
        [ Attr.style "width" "1500px"
        , Attr.style "height" "1500px"
        , Attr.style "perspective" "-200px"
        ]
        [ div
            [ Attr.style "width" "100%"
            , Attr.style "height" "100%"
            , Attr.style "position" "relative"
            , Attr.style "transform-style" "preserve-3d"
            ]
            [ div
                (animateFace model.page <|
                    \page ->
                        case page of
                            Checkboxes ->
                                Animator.at 0

                            AboutPageTransitions ->
                                Animator.at -1

                            ThatsIt ->
                                Animator.at -2
                )
                [ viewCheckboxDemo model ]
            , div
                (animateFace model.page <|
                    \page ->
                        case page of
                            Checkboxes ->
                                Animator.at 1

                            AboutPageTransitions ->
                                Animator.at 0

                            ThatsIt ->
                                Animator.at -1
                )
                [ viewPageTransitions model ]
            , div
                (animateFace model.page <|
                    \page ->
                        case page of
                            Checkboxes ->
                                Animator.at 2

                            AboutPageTransitions ->
                                Animator.at 1

                            ThatsIt ->
                                Animator.at 0
                )
                [ viewThatsIt model ]
            ]
        ]


animateFace page fn =
    let
        anim =
            Animator.details page fn
    in
    face anim.position


face i =
    [ Attr.style "width" "100%"
    , Attr.style "height" "100%"
    , Attr.style "position" "absolute"
    , Attr.style "background-color" "white"
    , Attr.style "transform" ("rotateY(" ++ String.fromFloat (i * 90) ++ "deg) translateZ(30px)")
    ]


viewPageTransitions model =
    div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"

        -- , Attr.style "width" "900px"
        ]
        [ h1 [] [ text "Page Transitions" ]
        , text "Animator.Timeline Page"
        ]


viewThatsIt model =
    div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "justify-content" "center"
        ]
        [ h1 [] [ text "Other cool pieces" ]
        , ul [ Attr.style "width" "900px" ]
            [ li [] [ text "Bezier Curve based smooth-interpolator" ]
            , li [] [ text "Dynamically created CSS Animations" ]
            , li []
                [ text "Sprite Animation"
                , img
                    [ Attr.width 100
                    , Attr.src "mario.gif"
                    ]
                    []
                ]
            ]
        ]


viewCheckboxDemo model =
    div []
        [ div
            [ Attr.style "display" "flex"
            , Attr.style "width" "100%"
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "center"
            ]
            [ if Animator.current model.transitions then
                div [] [ text "Animator.Timeline Bool" ]

              else
                div [] [ text "Bool" ]
            ]
        , div
            [ Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "center"
            , Attr.style "padding" "200px"
            ]
            [ --viewDescription model.checked
              -- ,
              viewHugeCheckbox model.spin model.checked
            ]
        ]


viewDescription timeline =
    div
        [ Attr.style "border-left" "1px solid blue"
        , Attr.style "border-right" "1px solid blue"
        , Attr.style "padding" "20px"
        , Attr.style "display" "flex"
        , Attr.style "height" "200px"
        , Attr.style "margin-bottom" "50px"
        , Attr.style "flex-direction" "row"
        ]
        (List.map describeEvent (Animator.describe timeline))


describeEvent description =
    case description of
        Internal.Timeline.DescribeStartTransition time ->
            Html.div []
                [ Html.text "start"
                ]

        Internal.Timeline.DescribeEvent time event ->
            Html.div
                [ Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "justify-content" "center"
                , Attr.style "margin" "16px"
                ]
                [ Html.span [] [ Html.text (eventToString event) ]
                , viewTime time
                ]

        Internal.Timeline.DescribeInterruption details ->
            Html.div
                [ Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "justify-content" "center"
                , Attr.style "margin" "32px"
                ]
                [ Html.span [] [ Html.text (eventToString details.target) ]
                , viewTime details.interruption
                , Html.span [] [ Html.text "⭣" ]
                , Html.span [] [ Html.text (eventToString details.newTarget) ]
                , viewTime details.newTargetTime
                ]


eventToString bool =
    case bool of
        True ->
            "True"

        False ->
            "False"


viewTime time =
    let
        ms =
            Time.posixToMillis time
    in
    text (String.right 5 (String.fromInt ms))


{-| Our actual checkbox!

We want to animate the checked state.

  - Animate the background color of the actual checkbox
  - Animate the opacity of the checkmark
  - Animate the rotation of the checkmark
  - Animate the scale of the checkmark

-}
viewHugeCheckbox spin checked =
    div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "flex-direction" "column"
        ]
        [ div
            [ Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "cursor" "pointer"
            , Events.onClick (Check (not (Animator.current checked)))
            ]
            [ div
                [ Animator.Inline.backgroundColor checked <|
                    \state ->
                        case state of
                            True ->
                                Color.rgb255 255 96 96

                            False ->
                                Color.white
                , Animator.Inline.borderColor checked <|
                    \state ->
                        case state of
                            True ->
                                Color.rgb255 255 96 96

                            False ->
                                Color.black
                , Attr.style "border-width" "10px"
                , Attr.style "border-style" "solid"
                , Attr.style "color" "#000"
                , Attr.style "width" "160px"
                , Attr.style "height" "160px"
                , Attr.style "border-radius" "20px"
                , Attr.style "font-size" "160px"
                , Attr.style "line-height" "1.0"
                , Attr.style "text-align" "center"
                ]
                [ div
                    [ Animator.Inline.opacity checked <|
                        \state ->
                            case state of
                                True ->
                                    1

                                False ->
                                    0
                    , Animator.Inline.transform
                        { position = { x = 0, y = 0 }
                        , rotate =
                            Animator.linear checked <|
                                \state ->
                                    case state of
                                        True ->
                                            turns 0

                                        False ->
                                            turns 0.25
                        , scale =
                            Animator.linear checked <|
                                \state ->
                                    case state of
                                        True ->
                                            1

                                        False ->
                                            0
                        }
                    ]
                    [ text "!" ]
                ]
            , span
                [ Attr.style "margin-left" "32px"
                , Attr.style "font-size" "190px"
                ]
                [ text "Click me" ]
            ]
        , div
            [ Animator.Inline.opacity checked <|
                \state ->
                    case state of
                        True ->
                            1

                        False ->
                            0
            ]
            [ text "Great job "
            , span
                [ Attr.style "display" "inline-block"
                , if Animator.current spin then
                    formatTransform <|
                        Animator.details checked <|
                            \state ->
                                case state of
                                    True ->
                                        Animator.wave -10 10
                                            |> Animator.loop (Animator.millis 1000)

                                    False ->
                                        Animator.at 0

                  else
                    Attr.class ""
                ]
                [ text "👍" ]
            ]
        ]


formatTransform movement =
    Attr.style "transform"
        ("translate(0px," ++ String.fromFloat movement.position ++ "px)")


viewCheckbox label checked onClick =
    div
        [ Attr.style "display" "flex"
        , Attr.style "align-items" "center"
        , Attr.style "cursor" "pointer"
        , Events.onClick (onClick (not (Animator.current checked)))
        ]
        [ div
            [ Animator.Inline.backgroundColor checked <|
                \state ->
                    case state of
                        True ->
                            Color.rgb255 255 96 96

                        False ->
                            Color.white
            , Animator.Inline.borderColor checked <|
                \state ->
                    case state of
                        True ->
                            Color.rgb255 255 96 96

                        False ->
                            Color.black
            , Attr.style "border-width" "2px"
            , Attr.style "border-style" "solid"
            , Attr.style "color" "#000"
            , Attr.style "width" "24px"
            , Attr.style "height" "24px"
            , Attr.style "border-radius" "4px"
            , Attr.style "font-size" "24px"
            , Attr.style "line-height" "1.0"
            , Attr.style "text-align" "center"
            ]
            [ div
                [ Animator.Inline.opacity checked <|
                    \state ->
                        case state of
                            True ->
                                1

                            False ->
                                0
                , Animator.Inline.transform
                    { position = { x = 0, y = 0 }
                    , rotate =
                        Animator.linear checked <|
                            \state ->
                                case state of
                                    True ->
                                        turns 0

                                    False ->
                                        turns 0.25
                    , scale =
                        Animator.linear checked <|
                            \state ->
                                case state of
                                    True ->
                                        1

                                    False ->
                                        0
                    }
                ]
                [ text "!" ]
            ]
        , span
            [ Attr.style "margin-left" "12px"
            ]
            [ text label ]
        ]
