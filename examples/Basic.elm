module Basic exposing (main, subscriptions, update, view)

import Animator
import Browser
import Color
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Time


main =
    Browser.document
        { init =
            \() ->
                update QueueThree
                    { timeline =
                        Animator.init Hufflepuff
                    , time = Time.millisToPosix 0
                    , house = Hufflepuff
                    }

        -- , Cmd.none
        -- )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { timeline : Animator.Timeline House
    }


type Msg
    = Tick (Animator.Timeline House)
    | NextHouse
    | QueueThree
    | NewTime Time.Posix


type House
    = Hufflepuff
    | Griffyndor
    | Slytherin
    | Ravenclaw


toPx x =
    "translate(" ++ String.fromFloat x ++ "px, 0)"


toHousePosition event =
    case event of
        Hufflepuff ->
            Animator.to 100

        Griffyndor ->
            Animator.to 400

        Slytherin ->
            Animator.to 700

        Ravenclaw ->
            Animator.to 1000


toHousePositionWithOrbit event =
    case event of
        Hufflepuff ->
            Animator.to 100

        Griffyndor ->
            Animator.orbit
                { point = 400
                , duration = Animator.millis 100
                , toPosition =
                    \u ->
                        20 * sin (u * (2 * pi))
                }

        Slytherin ->
            Animator.to 700

        Ravenclaw ->
            Animator.to 1000


view model =
    { title = "Elm - Select Harry Potter House"
    , body =
        let
            toPos =
                toHousePositionWithOrbit
        in
        [ Html.div
            []
            [ Html.button [ Events.onClick QueueThree ] [ Html.text "Queue Three" ]
            , Html.button [ Events.onClick NextHouse ] [ Html.text "Next" ]
            , Html.input
                [ Attr.type_ "range"
                , Attr.value (String.fromInt (Time.posixToMillis model.time))
                , Attr.min "0"
                , Attr.max "4000"
                , Attr.style "width" "500px"
                , Events.onInput
                    (\newTime ->
                        case String.toInt newTime of
                            Nothing ->
                                NewTime (Time.millisToPosix 0)

                            Just t ->
                                NewTime (Time.millisToPosix t)
                    )
                ]
                []
            ]
        , Html.div
            [ Attr.style "display" "flex"
            , Attr.style "width" "100%"
            , Attr.style "height" "800px"
            ]
            [ Html.div
                [ Attr.style "width" "200px"
                , Attr.style "height" "200px"
                , Attr.style "align-self" "center"
                , Attr.style "justify-self" "center"
                , Events.onClick NextHouse
                , Attr.style "transform"
                    (toPx
                        (Animator.move model.timeline toPos)
                    )
                , Attr.style "background-color"
                    (Color.toCssString
                        (Animator.color model.timeline <|
                            \event ->
                                case event of
                                    Hufflepuff ->
                                        yellow

                                    Griffyndor ->
                                        red

                                    Slytherin ->
                                        green

                                    Ravenclaw ->
                                        blue
                        )
                    )
                ]
                [ case Animator.moveMotion model.timeline toPos of
                    { position, velocity } ->
                        Html.div []
                            [ Html.div [] [ Html.text "pos: ", Html.text (String.fromFloat position) ]
                            , Html.div [] [ Html.text "vel: ", Html.text (String.fromFloat velocity) ]

                            -- , Html.div [] [ Html.text "between: ", Html.text (Debug.toString between) ]
                            ]
                ]
            ]
        ]
    }


next house =
    case house of
        Hufflepuff ->
            Griffyndor

        Griffyndor ->
            Slytherin

        Slytherin ->
            Ravenclaw

        Ravenclaw ->
            Hufflepuff


update msg model =
    case msg of
        Tick newTimeline ->
            ( { model | timeline = newTimeline }
            , Cmd.none
            )

        QueueThree ->
            let
                addToQueue _ ( q, house ) =
                    ( Animator.wait (Animator.seconds 0.5)
                        :: Animator.event (Animator.seconds 1) (next house)
                        :: q
                    , next house
                    )

                ( forQueue, newHouse ) =
                    List.foldl addToQueue ( [], model.house ) (List.range 1 3)
            in
            ( { model
                | timeline =
                    Animator.queue (List.reverse forQueue) model.timeline
                , house = newHouse
              }
            , Cmd.none
            )

        NextHouse ->
            let
                newHouse =
                    next model.house
            in
            ( { model
                | timeline =
                    Animator.queue [ Animator.event (Animator.seconds 1) newHouse ] model.timeline
                , house = newHouse
              }
            , Cmd.none
            )

        NewTime newPosix ->
            ( { model
                | time = newPosix
                , timeline =
                    Animator.update newPosix model.timeline
              }
            , Cmd.none
            )


subscriptions model =
    Sub.batch
        [--Animator.subscription Tick model.timeline
        ]


red =
    Color.rgb 1 0 0


green =
    Color.rgb 0 1 0


blue =
    Color.rgb 0 0 1


yellow =
    Color.rgb 1 1 0
