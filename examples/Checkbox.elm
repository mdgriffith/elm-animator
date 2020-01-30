module Checkbox exposing (main)

{-| -}

import Animator
import Animator.Inline
import Browser
import Color
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Time


main =
    Browser.document
        { init =
            \() ->
                ( { checked = Animator.init False
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                animator
                    |> Animator.toSubscription Tick model
        }


{-| This is the "animator", which is able to get and set each timeline you want animated.

It will turn

-}
animator : Animator.Animator Model
animator =
    Animator.animator
        -- we tell the animator how to get the checked timeline using .checked
        -- and we tell the animator how to update that timeline with updateChecked
        |> Animator.with .checked updateChecked


updateChecked newChecked model =
    { model | checked = newChecked }


{--}
type alias Model =
    { checked : Animator.Timeline Bool
    }


type Msg
    = Tick Time.Posix
    | Check Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( model
                |> Animator.update newTime animator
            , Cmd.none
            )

        Check bool ->
            ( { model
                | checked =
                    model.checked
                        |> Animator.to (Animator.millis 2000) bool
              }
            , Cmd.none
            )


view : Model -> Browser.Document Msg
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
                , Attr.style "flex-direction" "column"
                , Attr.style "align-items" "center"
                , Attr.style "justify-content" "center"
                , Attr.style "padding" "200px"
                ]
                [ viewHugeCheckbox model.checked
                ]
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
viewHugeCheckbox : Animator.Timeline Bool -> Html Msg
viewHugeCheckbox checked =
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
                        if state then
                            Color.rgb255 255 96 96

                        else
                            Color.white
                , Animator.Inline.borderColor checked <|
                    \state ->
                        if state then
                            Color.rgb255 255 96 96

                        else
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
                            if state then
                                1

                            else
                                0
                    , Animator.Inline.transform
                        { position = { x = 0, y = 0 }
                        , rotate =
                            Animator.linear checked <|
                                \state ->
                                    if state then
                                        turns 0

                                    else
                                        turns 0.25
                        , scale =
                            Animator.linear checked <|
                                \state ->
                                    if state then
                                        1

                                    else
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
                    if state then
                        1

                    else
                        0
            ]
            [ text "Great job "
            , span
                [ Attr.style "display" "inline-block"
                ]
                [ text "👍" ]
            ]
        ]
