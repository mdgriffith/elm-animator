module Checkbox exposing (main)

{-| -}

import Animator
import Animator.CSS
import Browser
import Color
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Time


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions =
            subscriptions

        -- \model -> Sub.none
        }


init () =
    ( { checked = Animator.init False }
    , Cmd.none
    )


updateChecked newChecked model =
    { model | checked = newChecked }


subscriptions model =
    Animator.animator Tick
        |> Animator.with .checked (\new m -> { m | checked = new })
        |> Animator.toSubscription model


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
            , Attr.style "align-items" "center"
            , Attr.style "justify-content" "center"
            , Attr.style "width" "100%"
            , Attr.style "height" "1000px"
            , Attr.style "font-family" "'Roboto', sans-serif"
            ]
            [ viewCheckbox model.checked
            ]
        ]
    }


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
                , Attr.style "user-select" "none"
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
                            Animator.float checked <|
                                \state ->
                                    case state of
                                        True ->
                                            turns 0

                                        False ->
                                            turns 0.4
                        , scale =
                            Animator.float checked <|
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
