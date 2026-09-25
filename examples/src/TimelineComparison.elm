module TimelineComparison exposing (main)

import Animator
import Animator.Timeline
import Animator.Transition
import Animator.Value as Value
import Browser
import Browser.Events
import Html
import Html.Attributes as Attr
import Html.Events
import Html.Keyed
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { timeline : Animator.Timeline.Timeline Box
    , historyOne : List Float
    , historyTwo : List Float
    }


type Msg
    = NewPosix Time.Posix
    | Start
    | BoxClicked Box
    | SequenceClicked


type Box
    = A
    | B
    | C


init : ( Model, Cmd Msg )
init =
    ( { timeline =
            Animator.Timeline.init B
      , historyOne = []
      , historyTwo = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPosix posix ->
            ( { model
                | timeline = Animator.Timeline.update posix model.timeline
              }
            , Cmd.none
            )

        Start ->
            init

        BoxClicked newBox ->
            ( { model
                | timeline = Animator.Timeline.to (Animator.ms 1000) newBox model.timeline
              }
            , Cmd.none
            )

        SequenceClicked ->
            ( { model
                | timeline =
                    model.timeline
                        |> Animator.Timeline.queue
                            [ Animator.Timeline.transitionTo (Animator.ms 1000) A
                            , Animator.Timeline.transitionTo (Animator.ms 1000) B
                            , Animator.Timeline.transitionTo (Animator.ms 1000) C
                            ]
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    -- if Animator.Timeline.hasChanges model.timeline then
    if Animator.Timeline.isRunning model.timeline then
        Browser.Events.onAnimationFrame NewPosix

    else
        Sub.none


pointer =
    Attr.style "cursor" "pointer"


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ column 40
            [ Attr.style "width" "100%", Attr.style "justify-content" "center", Attr.style "align-items" "center", Attr.style "margin-top" "200px" ]
            [ column 10
                []
                [ box [ pointer, Html.Events.onClick SequenceClicked ] []
                , cursor model.timeline
                , row []
                    [ box [ pointer, Html.Events.onClick (BoxClicked A) ] []
                    , box [ pointer, Html.Events.onClick (BoxClicked B) ] []
                    , box [ pointer, Html.Events.onClick (BoxClicked C) ] []
                    ]
                ]

            -- , rowKeyed []
            --     [ viewKeyframes model
            --     , viewValue model
            --     ]
            ]
        ]


rowKeyed : List (Html.Attribute msg) -> List ( String, Html.Html msg ) -> Html.Html msg
rowKeyed attributes columns =
    Html.Keyed.node "div"
        (Attr.style "display" "flex"
            :: Attr.style "flex-direction" "row"
            :: Attr.style "gap" "40px"
            -- :: Attr.style "width" "1000px"
            :: attributes
        )
        columns


viewKeyframes model =
    ( "keyframes"
    , Animator.div
        (Animator.onTimeline model.timeline
            (\state ->
                case state of
                    A ->
                        [ Animator.opacity 0.5
                        , Animator.x -200
                        , Animator.y 0
                        , Animator.rotation 0.75
                        ]

                    B ->
                        [ Animator.x 0
                        , Animator.y 200
                        ]

                    C ->
                        [ Animator.opacity 1
                        , Animator.x 200
                        , Animator.y 0
                        , Animator.rotation 0.5
                        ]
            )
        )
        [ Attr.style "width" "100px"
        , Attr.style "height" "100px"
        , Attr.style "border-radius" "12px"
        , Attr.style "background-color" "red"
        ]
        []
    )


viewValue model =
    let
        xy =
            Value.xy model.timeline <|
                \state ->
                    case state of
                        A ->
                            { x = Value.to -200
                            , y = Value.to 0
                            }

                        B ->
                            { x = Value.to 0
                            , y = Value.to 200
                            }

                        C ->
                            { x = Value.to 200
                            , y = Value.to 0
                            }

        rotation =
            Value.float model.timeline <|
                \state ->
                    case state of
                        A ->
                            Value.to 0.75

                        B ->
                            Value.to 0

                        C ->
                            Value.to 0.5

        opacity =
            Value.float model.timeline <|
                \state ->
                    case state of
                        A ->
                            Value.to 0.5

                        B ->
                            Value.to 1

                        C ->
                            Value.to 1
    in
    ( "value"
    , Html.div
        [ Attr.style "width" "100px"
        , Attr.style "height" "100px"
        , Attr.style "border-radius" "12px"
        , Attr.style "background-color" "blue"
        , Attr.style "opacity" (String.fromFloat opacity)
        , Attr.style "rotate" (String.fromFloat rotation ++ "turn")
        , Attr.style "translate"
            (String.fromFloat
                xy.x
                ++ "px "
                ++ String.fromFloat
                    xy.y
                ++ "px"
            )
        ]
        []
    )


cursor : Animator.Timeline.Timeline Box -> Html.Html Msg
cursor timeline =
    column 4
        []
        [ viewCursorValue timeline
        , viewCursorKeyframes timeline
        ]



-- Html.text ""


viewCursorKeyframes timeline =
    Animator.div
        (Animator.onTimeline timeline
            (\state ->
                case state of
                    A ->
                        [ Animator.x 40
                            |> Animator.withTransition
                                (Animator.Transition.spring
                                    { wobble = 1
                                    , quickness = 0
                                    }
                                )
                        ]

                    B ->
                        [ Animator.x 185
                            |> Animator.withTransition
                                (Animator.Transition.spring
                                    { wobble = 1
                                    , quickness = 0
                                    }
                                )
                        ]

                    C ->
                        [ Animator.x 322
                            |> Animator.withTransition
                                (Animator.Transition.spring
                                    { wobble = 1
                                    , quickness = 0
                                    }
                                )
                        ]
            )
        )
        [ Attr.style "width" "10px"
        , Attr.style "height" "10px"
        , Attr.style "border-radius" "50%"
        , Attr.style "background-color" "red"
        ]
        []


viewCursorValue timeline =
    -- Animator.div
    Html.div
        -- (Animator.onTimeline timeline
        --     (\state ->
        --         case state of
        --             A ->
        --                 [ Animator.x 40
        --                 ]
        --             B ->
        --                 [ Animator.x 185
        --                 ]
        --             C ->
        --                 [ Animator.x 322
        --                 ]
        --     )
        -- )
        [ Attr.style "width" "10px"
        , Attr.style "height" "10px"
        , Attr.style "border-radius" "50%"
        , Attr.style "background-color" "blue"
        , Attr.style "transform"
            ("translateX("
                ++ String.fromFloat
                    (Value.float timeline <|
                        \state ->
                            case state of
                                A ->
                                    Value.to 40
                                        |> Value.withTransition
                                            (Animator.Transition.spring
                                                { wobble = 1
                                                , quickness = 0
                                                }
                                            )

                                B ->
                                    Value.to 185
                                        |> Value.withTransition
                                            (Animator.Transition.spring
                                                { wobble = 1
                                                , quickness = 0
                                                }
                                            )

                                C ->
                                    Value.to 322
                                        |> Value.withTransition
                                            (Animator.Transition.spring
                                                { wobble = 1
                                                , quickness = 0
                                                }
                                            )
                    )
                ++ "px)"
            )
        ]
        []


row : List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
row attributes columns =
    Html.div
        (Attr.style "display" "flex"
            :: Attr.style "flex-direction" "row"
            :: Attr.style "gap" "40px"
            -- :: Attr.style "width" "1000px"
            :: attributes
        )
        columns


column : Int -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
column gap attributes columns =
    Html.div
        (Attr.style "display" "flex"
            :: Attr.style "flex-direction" "column"
            :: Attr.style "gap" (String.fromInt gap ++ "px")
            :: attributes
        )
        columns


box : List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
box attributes content =
    Html.div
        (Attr.style "width" "100px"
            :: Attr.style "height" "100px"
            :: Attr.style "border-radius" "12px"
            :: Attr.style "background-color" "black"
            :: attributes
        )
        content


positionX : Float -> Html.Attribute msg
positionX x =
    Attr.style "transform" ("translateX(" ++ String.fromFloat x ++ "px)")
