module TimelineValue exposing (main)

import Animator
import Animator.Timeline
import Animator.Transition
import Animator.Value
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
    { timeline : Animator.Timeline.Timeline Float
    , historyOne : List Float
    , historyTwo : List Float
    }


type Msg
    = NewPosix Time.Posix
    | Start


init : ( Model, Cmd Msg )
init =
    let
        initialTimeline : Animator.Timeline.Timeline Float
        initialTimeline =
            Animator.Timeline.init 10

        queuedSteps : List (Animator.Timeline.Step Float)
        queuedSteps =
            [ Animator.Timeline.transitionTo (Animator.ms 1000) 100
            , Animator.Timeline.transitionTo (Animator.ms 1000) 50
            , Animator.Timeline.transitionTo (Animator.ms 1000) 5
            ]

        timelineWithSteps : Animator.Timeline.Timeline Float
        timelineWithSteps =
            Animator.Timeline.scale 3 <| Animator.Timeline.queue queuedSteps initialTimeline
    in
    ( { timeline = timelineWithSteps
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
                , historyOne = Animator.Value.float model.timeline Animator.Value.to :: model.historyOne
                , historyTwo = Animator.Value.float model.timeline (Animator.Value.withTransition Animator.Transition.linear << Animator.Value.to) :: model.historyTwo
              }
            , Cmd.none
            )

        Start ->
            init


subscriptions : Model -> Sub Msg
subscriptions model =
    if Animator.Timeline.isRunning model.timeline then
        Browser.Events.onAnimationFrame NewPosix

    else
        Sub.none


view : Model -> Html.Html Msg
view model =
    let
        positionStandard : Float
        positionStandard =
            Animator.Value.float model.timeline Animator.Value.to

        positionLinear : Float
        positionLinear =
            Animator.Value.float model.timeline (Animator.Value.withTransition Animator.Transition.linear << Animator.Value.to)
    in
    Html.div []
        [ row [ Attr.style "width" "600px", Attr.style "justify-content" "center", Attr.style "align-items" "center" ]
            [ column [ Attr.style "align-self" "flex-start", Attr.style "width" "200px", Attr.style "margin-top" "50px" ]
                [ Html.div []
                    [ Html.div [] [ Html.text ("Position Standard: " ++ String.fromFloat positionStandard) ]
                    , Html.div [] [ Html.text ("Position Linear: " ++ String.fromFloat positionLinear) ]
                    ]
                , Html.button [ Html.Events.onClick Start ] [ Html.text "Re-Start" ]
                ]
            , column [ Attr.style "width" "200px" ]
                (List.map (\x -> circle [ positionX x ] []) model.historyOne)
            , column [ Attr.style "width" "200px" ]
                (List.map (\x -> circle [ positionX x ] []) model.historyTwo)
            ]
        ]


row : List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
row attributes columns =
    Html.div
        (Attr.style "display" "flex"
            :: Attr.style "flex-direction" "row"
            :: Attr.style "gap" "10px"
            :: attributes
        )
        columns


column : List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
column attributes columns =
    Html.div
        (Attr.style "display" "flex"
            :: Attr.style "flex-direction" "column"
            :: Attr.style "gap" "10px"
            :: attributes
        )
        columns


circle : List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
circle attributes content =
    Html.div
        (Attr.style "width" "2px"
            :: Attr.style "height" "2px"
            :: Attr.style "border-radius" "50%"
            :: Attr.style "background-color" "black"
            :: attributes
        )
        content


positionX : Float -> Html.Attribute msg
positionX x =
    Attr.style "transform" ("translateX(" ++ String.fromFloat x ++ "px)")
