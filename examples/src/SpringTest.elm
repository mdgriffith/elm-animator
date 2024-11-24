module SpringTest exposing (..)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Time


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Vec2 =
    { x : Float, y : Float }


type alias Model =
    { targetPosition : Vec2
    , currentPosition : Vec2
    , velocity : Vec2
    , isClicked : Bool
    , time : Time.Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { targetPosition = { x = 0, y = 0 }
      , currentPosition = { x = 0, y = 0 }
      , velocity = { x = 0, y = 0 }
      , isClicked = False
      , time = Time.millisToPosix 0
      }
    , Cmd.none
    )


type Msg
    = Click Vec2
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click position ->
            ( { model
                | targetPosition = position
                , isClicked = True
              }
            , Cmd.none
            )

        Tick now ->
            if Time.posixToMillis model.time == 0 then
                ( { model
                    | time = now
                  }
                , Cmd.none
                )

            else if model.currentPosition == model.targetPosition then
                ( { model
                    | velocity = { x = 0, y = 0 }
                    , isClicked = False
                  }
                , Cmd.none
                )

            else
                let
                    dt =
                        toFloat (Time.posixToMillis now - Time.posixToMillis model.time)

                    damping =
                        12.0

                    springForce =
                        100.0

                    acceleration =
                        { x = (model.targetPosition.x - model.currentPosition.x) * springForce - damping * model.velocity.x
                        , y = (model.targetPosition.y - model.currentPosition.y) * springForce - damping * model.velocity.y
                        }

                    newVelocity =
                        { x = model.velocity.x + acceleration.x * dt
                        , y = model.velocity.y + acceleration.y * dt
                        }

                    newPosition =
                        { x = model.currentPosition.x + model.velocity.x * dt
                        , y = model.currentPosition.y + model.velocity.y * dt
                        }
                in
                ( { model
                    | currentPosition = newPosition
                    , velocity = newVelocity
                    , time = now
                  }
                , Cmd.none
                )


view : Model -> Html Msg
view model =
    div
        [ Html.Events.on "click" (Decode.map Click decodeClick)
        , Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "justify-content" "center"
        , Html.Attributes.style "align-items" "center"
        ]
        [ div
            [ Html.Attributes.style "width" "50px"
            , Html.Attributes.style "height" "50px"
            , Html.Attributes.style "background-color" "blue"
            , Html.Attributes.style "border-radius" "50%"
            , Html.Attributes.style "transform" ("translate(" ++ String.fromFloat (model.currentPosition.x - 25) ++ "px, " ++ String.fromFloat (model.currentPosition.y - 25) ++ "px)")
            ]
            []
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isClicked then
        Time.every 16 Tick

    else
        Sub.none


decodeClick : Decode.Decoder Vec2
decodeClick =
    Decode.map2 Vec2
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)
