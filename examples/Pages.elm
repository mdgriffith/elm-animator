module Pages exposing (main)

{- 
**IN PROGRESS EXAMPLE, NOT QUITE FINISHED :D **

   This example shows three techniques.

   - 1. Page transitions where the url is modified.
       We essentially have a `Animator.Timeline Page`

   - 2. General UI state!
       -> hover + focus

-}


import Animator
import Animator.Css
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
        |> Animator.Css.with .checked updateChecked


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
                    Animator.slowly bool model.checked
                        
                        -- |> Animator.toOver (Animator.millis 2000) bool
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
            , Attr.style "font-size" "48px"
            , Attr.style "user-select" "none"
            , Attr.style "padding" "50px"
            , Attr.style "box-sizing" "border-box"
            , Attr.style "font-family" "'Roboto', sans-serif"
            ]
            [ div
                [ Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "align-items" "center"
                , Attr.style "justify-content" "center"
                , Attr.style "padding" "200px"
                ]
                [ case Animator.current model.checked of
                    True ->
                        Html.text "True"

                    False ->
                        Html.text "False"

                , case Animator.previous model.checked of
                    True ->
                        Html.text "Prev- True"

                    False ->
                        Html.text "Prev- False"
                , viewHugeCheckbox model.checked
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
            , Attr.style "perspective" "500px"
            , Events.onClick (Check (not (Animator.current checked)))
            ]
            [ Animator.Css.animated checked
                [ Animator.Css.backgroundColor <|
                    \state ->
                        if state then
                            Color.rgb255 255 96 96

                        else
                            Color.white
                , Animator.Css.borderColor <|
                    \state ->
                        if state then
                            Color.rgb255 255 96 96

                        else
                            Color.black
                , Animator.Css.transformWith
                    { rotationAxis =
                        { x = 0
                        , y = 0.25
                        , z = 0.25
                        }
                    , origin = Animator.Css.center
                    }
                  <|
                    \on ->
                        if on then
                            -- Animator.Css.xy
                            --     { x = 0
                            --     , y = 0
                            --     }
                            -- Animator.Css.rotateTo (turns 0.125)
                            Animator.Css.xy 
                                { x = 0
                                , y = 0
                                }
                            --     -- Animator.Css.transform
                            --     |>
                            -- Animator.Css.rotating (Animator.seconds 5)
                            -- { x =
                            -- Animator.wave -500 -620
                            --     |> Animator.loop (Animator.millis 1999)
                            -- , y =
                            --     Animator.wave -500 -620
                            --         |> Animator.loop (Animator.millis 1999)
                            -- }

                        else
                            -- Animator.Css.xy
                            -- -- { x =
                            -- --     Animator.wave -200 -300
                            -- --         |> Animator.loop (Animator.millis 1999)
                            -- -- , y =
                            -- --     Animator.wave -200 -300
                            -- --         |> Animator.loop (Animator.millis 1999)
                            -- -- }
                            -- { x = 200
                            -- , y = 60
                            -- }
                            -- |>
                            -- Animator.Css.rotateTo (turns 0)
                            Animator.Css.xy 
                                { x = 0
                                , y = 0
                                }
                                |> Animator.Css.lookAt 
                                    { x = 0.7
                                    , y = 0.2
                                    , z = 0.8
                                    
                                    }

                ]
                [ Attr.style "border-width" "10px"
                , Attr.style "border-style" "solid"
                , Attr.style "color" "#000"
                , Attr.style "width" "160px"
                , Attr.style "height" "160px"
                , Attr.style "border-radius" "20px"
                , Attr.style "font-size" "160px"
                , Attr.style "line-height" "1.0"
                , Attr.style "text-align" "center"
                , Attr.style "perspective" "500px"
                ]
                [ Html.text "!"
                ]
            , span
                [ Attr.style "margin-left" "32px"
                , Attr.style "font-size" "190px"
                ]
                [ text "Click me" ]
            ]
        , div
            [ 
                -- Animator.Css.opacity checked <|
                -- \state ->
                --     if state then
                --         1

                --     else
                --         0
            ]
            [ text "Great job "
            , span
                [ Attr.style "display" "inline-block"
                ]
                [ text "👍" ]
            ]
        ]
