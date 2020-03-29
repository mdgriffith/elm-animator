module Bezier exposing (main)

{-| -}

import Browser
import Browser.Events
import Html
import Html.Attributes as Attr
import Html.Events
import Json.Decode as Decode
import Svg exposing (..)
import Svg.Attributes exposing (..)


main =
    Browser.element
        { init =
            \() ->
                ( { moving = NoMovement
                  , one = Point 0 0.5
                  , controlOne = Point 0.4 0.5
                  , controlTwo = Point 0.2 0
                  , two = Point 1 0
                  }
                , Cmd.none
                )
        , view = view
        , update =
            update
        , subscriptions =
            subscriptions
        }


subscriptions model =
    case model.moving of
        NoMovement ->
            Sub.none

        _ ->
            Sub.batch
                [ Browser.Events.onMouseUp (Decode.succeed EndMovement)
                , Browser.Events.onMouseMove
                    (Decode.map MouseCoords
                        (Decode.map2
                            Point
                            (Decode.field "pageX" Decode.float)
                            (Decode.field "pageY" Decode.float)
                        )
                    )
                ]


controlOneFull =
    Point 1 0.5


controlTwoFull =
    Point 0 0


type Movement
    = NoMovement
    | Start Entity
    | Moving
        { entity : Entity
        , startedAt : Point
        , currentlyAt : Point
        }


type Entity
    = Top
    | Bottom


type Msg
    = StartMovementForTop
    | StartMovementForBottom
    | MouseCoords Point
    | EndMovement
    | ResetTo Coords


type alias Coords =
    { one : Point
    , controlOne : Point
    , controlTwo : Point
    , two : Point
    }


linear =
    { one = Point 0 0.5
    , controlOne = Point 0 0.5
    , controlTwo = Point 1 0
    , two = Point 1 0
    }


default =
    { one = Point 0 0.5
    , controlOne = Point 0.4 0.5
    , controlTwo = Point 0.2 0
    , two = Point 1 0
    }


type alias Point =
    { x : Float
    , y : Float
    }


update msg model =
    case msg of
        StartMovementForTop ->
            ( { model | moving = Start Top }
            , Cmd.none
            )

        StartMovementForBottom ->
            ( { model | moving = Start Bottom }
            , Cmd.none
            )

        MouseCoords coords ->
            case model.moving of
                NoMovement ->
                    ( model
                    , Cmd.none
                    )

                Start entity ->
                    ( { model
                        | moving =
                            Moving
                                { entity = entity
                                , startedAt = coords
                                , currentlyAt = coords
                                }
                      }
                    , Cmd.none
                    )

                Moving xy ->
                    ( { model
                        | moving =
                            Moving { xy | currentlyAt = coords }
                      }
                    , Cmd.none
                    )

        EndMovement ->
            ( { model
                | moving = NoMovement
                , controlOne = adjust Bottom model.moving model.controlOne
                , controlTwo = adjust Top model.moving model.controlTwo
              }
            , Cmd.none
            )

        ResetTo coords ->
            ( { model
                | one = coords.one
                , controlOne = coords.controlOne
                , controlTwo = coords.controlTwo
                , two = coords.two
              }
            , Cmd.none
            )


view model =
    let
        controlOne =
            adjust Bottom model.moving model.controlOne

        controlTwo =
            adjust Top model.moving model.controlTwo
    in
    Html.div [ Attr.style "user-select" "none" ]
        [ Html.div
            [ Attr.style "display" "flex"
            , Attr.style "flex-direction" "row"
            , Attr.style "width" "600px"
            , Attr.style "box-sizing" "border-box"
            , Attr.style "justify-content" "center"
            , Attr.style "padding-top" "50px"
            ]
            [ Html.button
                [ Attr.style "border" "2px solid black"
                , Attr.style "border-radius" "3px"
                , Attr.style "background-color" "white"
                , Attr.style "cursor" "pointer"
                , Attr.style "padding" "8px 12px"
                , Html.Events.onClick (ResetTo linear)
                ]
                [ Html.text "Linear" ]
            , Html.button
                [ Attr.style "border" "2px solid black"
                , Attr.style "border-radius" "3px"
                , Attr.style "background-color" "white"
                , Attr.style "cursor" "pointer"
                , Attr.style "padding" "8px 12px"
                , Html.Events.onClick (ResetTo default)
                , Attr.style "margin-left" "20px"
                ]
                [ Html.text "Elm Animator Default" ]
            ]
        , svg
            [ width "800"
            , height "1000"
            , viewBox "-0.2 -0.2 1.8 1.8"
            ]
            [ Svg.path
                [ fill "none"
                , stroke "black"
                , strokeWidth "0.005"
                , d (renderPath { model | controlOne = controlOne, controlTwo = controlTwo })
                ]
                []
            , viewLine model.one controlOne
            , viewLine model.two controlTwo
            , viewScale model.one controlOneFull "Leave Smoothly"
                |> move 0 0.15
            , viewScale model.two controlTwoFull "Arrive Smoothly"
                |> move 0 -0.15
            , viewCircle model.one "Start"
            , viewMovableCircle
                { onMouseDown = StartMovementForBottom
                , isMoving = somethingMoving model.moving
                , point = controlOne
                , toLabel = fromFloat << .x
                }
            , viewCircle model.two "End"
            , viewMovableCircle
                { onMouseDown = StartMovementForTop
                , isMoving = somethingMoving model.moving
                , point = controlTwo
                , toLabel = \p -> fromFloat (1 - p.x)
                }
            ]
        ]


somethingMoving moving =
    case moving of
        Moving _ ->
            True

        _ ->
            False


fromFloat f =
    String.fromFloat f
        |> String.left 4


adjust label moving point =
    case moving of
        NoMovement ->
            point

        Start _ ->
            point

        Moving movement ->
            if movement.entity == label then
                { x =
                    point.x
                        - (factor * (movement.startedAt.x - movement.currentlyAt.x))
                        |> Basics.max 0
                        |> Basics.min 1
                , y = point.y
                }

            else
                point


factor =
    1.8 / 800


viewCircle point label =
    g []
        [ circle
            [ cx (String.fromFloat point.x)
            , cy (String.fromFloat point.y)
            , r "0.02"
            ]
            []
        , text_
            [ fontSize "0.05"
            , x (String.fromFloat point.x)
            , y (String.fromFloat (point.y + 0.065))
            , textAnchor "middle"
            ]
            [ text label ]
        ]


viewMovableCircle details =
    g
        [ Html.Events.onMouseDown details.onMouseDown
        , Attr.style "cursor"
            (if details.isMoving then
                "grabbing"

             else
                "grab"
            )
        ]
        [ circle
            [ cx (String.fromFloat details.point.x)
            , cy (String.fromFloat details.point.y)
            , r "0.02"
            , fill "red"
            ]
            []
        , text_
            [ fontSize "0.05"
            , x (String.fromFloat details.point.x)
            , y (String.fromFloat (details.point.y - 0.04))
            , textAnchor "middle"
            ]
            [ text (details.toLabel details.point) ]
        ]


viewLine one two =
    line
        [ x1 (String.fromFloat one.x)
        , y1 (String.fromFloat one.y)
        , x2 (String.fromFloat two.x)
        , y2 (String.fromFloat two.y)
        , stroke "black"
        , strokeWidth "0.005"
        , strokeDasharray "0.01 0.01"
        ]
        []


move x y content =
    g
        [ transform
            ("translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")")
        ]
        [ content ]


viewScale one two label =
    g
        []
        [ line
            [ x1 (String.fromFloat one.x)
            , y1 (String.fromFloat one.y)
            , x2 (String.fromFloat two.x)
            , y2 (String.fromFloat two.y)
            , stroke "#EEE"
            , strokeWidth "0.005"
            ]
            []
        , viewText
            { width = 0.35
            , height = 0.05
            , label = label
            , x = abs (two.x - one.x) / 2
            , y = one.y
            }
        , viewText
            { width = 0.05
            , height = 0.05
            , label = "0"
            , x = one.x
            , y = one.y
            }
        , viewText
            { width = 0.05
            , height = 0.05
            , label = "1"
            , x = two.x
            , y = two.y
            }
        ]


viewText details =
    g []
        [ rect
            [ x
                (String.fromFloat
                    (details.x
                        - (details.width / 2)
                    )
                )
            , y
                (String.fromFloat
                    (details.y - (details.height / 2))
                )
            , width (String.fromFloat details.width)
            , height (String.fromFloat details.height)
            , fill "white"
            ]
            []
        , text_
            [ fontSize "0.05"
            , x
                (String.fromFloat
                    details.x
                )
            , y
                (String.fromFloat
                    ((details.y + (details.height / 2)) - 0.01)
                )
            , textAnchor "middle"
            ]
            [ text details.label ]
        ]


renderPath points =
    ("M " ++ renderPoint points.one)
        ++ (" C "
                ++ ([ points.controlOne
                    , points.controlTwo
                    , points.two
                    ]
                        |> List.map renderPoint
                        |> String.join " "
                   )
           )


renderPoint point =
    String.fromFloat point.x ++ "," ++ String.fromFloat point.y
