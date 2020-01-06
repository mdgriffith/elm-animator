module Checkbox exposing (main)

{-| -}

import Animator
import Animator.CSS
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
                ( { checked = Animator.init False }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


{-| This is the "animator", which is able to get and set each timeline you want animated.

It will turn

-}
subscriptions model =
    Animator.animator Tick
        -- we tell the animator how to get the checked timeline using .checked
        -- and we tell the animator how to update that timeline with updateChecked
        |> Animator.with .checked updateChecked
        |> Animator.toSubscription model


updateChecked newChecked model =
    { model | checked = newChecked }


{--}
type alias Model =
    { checked : Animator.Timeline Bool
    }


type Msg
    = Tick Model
    | Check Bool


update msg model =
    case msg of
        Tick newModel ->
            ( newModel
            , Cmd.none
            )

        Check bool ->
            ( { checked =
                    Animator.interrupt
                        [ Animator.event (Animator.millis 2000) bool
                        ]
                        model.checked
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
            [ Attr.style "display" "flex"
            , Attr.style "flex-direction" "column"
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "center"
            , Attr.style "width" "100%"
            , Attr.style "height" "1000px"
            , Attr.style "user-select" "none"
            , Attr.style "font-family" "'Roboto', sans-serif"
            ]
            [ viewDescription model.checked
            , viewCheckbox model.checked
            ]
        ]
    }


viewDescription timeline =
    div
        [ Attr.style "border-left" "1px solid blue"
        , Attr.style "border-right" "1px solid blue"
        , Attr.style "padding" "20px"
        , Attr.style "display" "flex"
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
viewCheckbox checked =
    div []
        [ div
            [ Attr.style "display" "flex"
            , Attr.style "align-items" "center"
            , Attr.style "cursor" "pointer"
            , Events.onClick (Check (not (Animator.current checked)))
            ]
            [ div
                [ Animator.CSS.backgroundColor checked <|
                    \state ->
                        case state of
                            True ->
                                Color.rgb255 255 96 96

                            False ->
                                Color.white
                , Animator.CSS.borderColor checked <|
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
                    [ Animator.CSS.opacity checked <|
                        \state ->
                            case state of
                                True ->
                                    1

                                False ->
                                    0
                    , Animator.CSS.transform
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
            [ Animator.CSS.opacity checked <|
                \state ->
                    case state of
                        True ->
                            1

                        False ->
                            0
            ]
            [ text "Great job 👍"
            ]
        ]
