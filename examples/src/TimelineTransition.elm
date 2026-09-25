module TimelineTransition exposing (main)

import Animator
import Animator.Timeline
import Animator.Transition
import Animator.Value as Value
import Browser
import Browser.Events
import Html
import Html.Attributes as Attr
import Html.Events
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
    { timeline : Box
    , historyOne : List Float
    , historyTwo : List Float
    }


type Msg
    = NewPosix Time.Posix
    | Start
    | BoxClicked Box


type Box
    = A
    | B
    | C


init : ( Model, Cmd Msg )
init =
    ( { timeline = B
      , historyOne = []
      , historyTwo = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPosix posix ->
            ( model
            , Cmd.none
            )

        Start ->
            init

        BoxClicked newBox ->
            ( { model
                | timeline = newBox
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    -- if Animator.Timeline.hasChanges model.timeline then
    --     -- if Animator.Timeline.isRunning model.timeline then
    --     Browser.Events.onAnimationFrame NewPosix
    -- else
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
                [ cursor model.timeline
                , row []
                    [ box [ pointer, Html.Events.onClick (BoxClicked A) ] []
                    , box [ pointer, Html.Events.onClick (BoxClicked B) ] []
                    , box [ pointer, Html.Events.onClick (BoxClicked C) ] []
                    ]
                ]
            , Animator.div
                (Animator.transition (Animator.ms 5000) <|
                    case model.timeline of
                        A ->
                            [ Animator.opacity 1
                            , Animator.x -200
                            , Animator.y 0
                            , Animator.rotation 0.75
                            ]

                        B ->
                            [ Animator.opacity 0.5
                            , Animator.x 0
                            , Animator.y 200
                            , Animator.rotation 0
                            ]

                        C ->
                            [ Animator.opacity 1
                            , Animator.x 200
                            , Animator.y 0
                            , Animator.rotation 0.5
                            ]
                )
                [ Attr.style "width" "100px"
                , Attr.style "height" "100px"
                , Attr.style "border-radius" "12px"
                , Attr.style "background-color" "black"
                ]
                []
            ]
        ]


cursor : Box -> Html.Html Msg
cursor timeline =
    Animator.div
        (Animator.transition (Animator.ms 5000)
            (case timeline of
                A ->
                    [ Animator.x 40
                    ]

                B ->
                    [ Animator.x 185
                    ]

                C ->
                    [ Animator.x 322
                    ]
            )
        )
        [ Attr.style "width" "10px"
        , Attr.style "height" "10px"
        , Attr.style "border-radius" "50%"
        , Attr.style "background-color" "red"
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
