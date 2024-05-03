module AnimationX exposing (main)

import Animator
import Animator.Timeline
import Animator.Transition
import Animator.Value
import Browser
import Browser.Events
import Html
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
    { timeline : Animator.Timeline.Timeline Float, isAnimating : Bool }


type Msg
    = NewPosix Time.Posix
    | StopAnimation


init : ( Model, Cmd Msg )
init =
    let
        initialTimeline : Animator.Timeline.Timeline number
        initialTimeline =
            Animator.Timeline.init 10

        queuedSteps : List (Animator.Timeline.Step number)
        queuedSteps =
            [ Animator.Timeline.transitionTo (Animator.ms 1000) 100
            , Animator.Timeline.transitionTo (Animator.ms 1000) 50
            , Animator.Timeline.transitionTo (Animator.ms 1000) 5
            ]

        timelineWithSteps : Animator.Timeline.Timeline number
        timelineWithSteps =
            Animator.Timeline.scale 3 <| Animator.Timeline.queue queuedSteps initialTimeline
    in
    ( { timeline = timelineWithSteps, isAnimating = True }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPosix posix ->
            ( { model | timeline = Animator.Timeline.update posix model.timeline }, Cmd.none )

        StopAnimation ->
            ( { model | isAnimating = False }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isAnimating then
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
        [ Html.div []
            [ Html.div [] [ Html.text ("Position Standard: " ++ String.fromFloat positionStandard) ]
            , Html.div [] [ Html.text ("Position Linear: " ++ String.fromFloat positionLinear) ]
            ]
        , Html.button [ Html.Events.onClick StopAnimation ] [ Html.text "Stop" ]
        ]
