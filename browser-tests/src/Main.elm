module Main exposing (main)

import Animator
import Animator.Timeline as Timeline
import Animator.Transition as Transition
import Browser
import Color
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Time


type alias Flags =
    { renderer : String
    , scenario : String
    }


type alias Model =
    { renderer : String
    , scenario : String
    , timeline : Timeline.Timeline (List Animator.Attribute)
    , updates : Int
    , target : Float
    }


type Msg
    = UnrelatedUpdate
    | Trigger
    | SetTarget Float


main : Program Flags Model Msg
main =
    Browser.element
        { init = \flags -> ( init flags, Cmd.none )
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


init : Flags -> Model
init flags =
    { renderer = flags.renderer
    , scenario = flags.scenario
    , updates = 0
    , target = 0
    , timeline =
        case flags.scenario of
            "initial" ->
                Timeline.init
                    [ Animator.opacity 0.35
                    , Animator.x 12
                    , Animator.y 34
                    , Animator.z 56
                    , Animator.scale 2
                    , Animator.color "background-color" (Color.rgb255 200 50 25)
                    ]

            "queued" ->
                Timeline.init [ Animator.opacity 1 ]
                    |> Timeline.queue
                        [ Timeline.transitionTo (Animator.ms 1000) (opacity 0.5)
                        , Timeline.wait (Animator.ms 500)
                        , Timeline.transitionTo (Animator.ms 1000) (opacity 0)
                        ]
                    |> Timeline.update (Time.millisToPosix 10000)

            "mixed-axis" ->
                Timeline.init [ Animator.x 0, Animator.y 0, Animator.z 30, Animator.opacity 1 ]
                    |> Timeline.to (Animator.ms 1000)
                        [ Animator.x 100 |> Animator.withTransition Transition.linear
                        , Animator.y 100 |> Animator.withTransition (Transition.bezier 0.5 0 0.5 0)
                        , Animator.z 30
                        , Animator.opacity 0.5 |> Animator.withTransition Transition.linear
                        ]
                    |> Timeline.update (Time.millisToPosix 10000)

            "spring-interruption" ->
                Timeline.init [ Animator.x 0 ]
                    |> Timeline.to (Animator.ms 1000) [ Animator.x 200 |> Animator.withTransition Transition.linear ]
                    |> Timeline.update (Time.millisToPosix 10000)

            "gc" ->
                gcTimeline

            "gc-delayed" ->
                gcTimeline

            "color" ->
                Timeline.init [ Animator.color "background-color" (Color.rgb255 255 0 0) ]
                    |> Timeline.to (Animator.ms 1000)
                        [ Animator.color "background-color" (Color.rgb255 0 0 255) |> Animator.withTransition Transition.linear ]
                    |> Timeline.update (Time.millisToPosix 10000)

            _ ->
                Timeline.init [ Animator.opacity 0 ]
                    |> Timeline.to (Animator.ms 1000) (opacity 1)
                    |> Timeline.update (Time.millisToPosix 10000)
    }


gcTimeline : Timeline.Timeline (List Animator.Attribute)
gcTimeline =
    Timeline.init [ Animator.opacity 0 ]
        |> Timeline.queue
            [ Timeline.transitionTo (Animator.ms 1000) (opacity 0.25)
            , Timeline.transitionTo (Animator.ms 1000) (opacity 0.5)
            , Timeline.transitionTo (Animator.ms 1000) (opacity 0.75)
            , Timeline.wait (Animator.ms 500)
            , Timeline.transitionTo (Animator.ms 10000) (opacity 1)
            ]
        |> Timeline.update (Time.millisToPosix 10000)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTarget target ->
            ( { model | target = target, updates = model.updates + 1 }, Cmd.none )

        UnrelatedUpdate ->
            ( { model | updates = model.updates + 1 }, Cmd.none )

        Trigger ->
            let
                now =
                    if model.scenario == "resting" then
                        11500

                    else
                        10500
            in
            ( { model
                | updates = model.updates + 1
                , target = 1
                , timeline =
                    if model.scenario == "gc" || model.scenario == "gc-delayed" then
                        model.timeline |> Timeline.update (Time.millisToPosix 18250)

                    else if model.scenario == "spring-interruption" then
                        model.timeline
                            |> Timeline.update (Time.millisToPosix now)
                            |> Timeline.to (Animator.ms 1000)
                                [ Animator.x 100 |> Animator.withTransition (Transition.spring { wobble = 1, quickness = 0 }) ]
                            |> Timeline.update (Time.millisToPosix now)

                    else
                        model.timeline
                            |> Timeline.update (Time.millisToPosix now)
                            |> Timeline.to (Animator.ms 1000) (opacity 0.75)
                            |> Timeline.update (Time.millisToPosix now)
              }
            , Cmd.none
            )


opacity : Float -> List Animator.Attribute
opacity value =
    [ Animator.opacity value |> Animator.withTransition Transition.linear ]


view : Model -> Html Msg
view model =
    Html.div []
        [ Animator.div
            (animation model)
            [ Attr.id "subject"
            , Attr.style "width" "100px"
            , Attr.style "height" "100px"
            ]
            [ Html.text "Animated" ]
        , Html.button [ Attr.id "update", Events.onClick UnrelatedUpdate ] [ Html.text "Unrelated update" ]
        , Html.button [ Attr.id "trigger", Events.onClick Trigger ] [ Html.text "Change target" ]
        , Html.button [ Attr.id "return", Events.onClick (SetTarget 0) ] [ Html.text "Return to zero" ]
        , Html.button [ Attr.id "retarget", Events.onClick (SetTarget 1.5) ] [ Html.text "Retarget" ]
        , Html.div [ Attr.id "updates" ] [ Html.text (String.fromInt model.updates) ]
        ]


animation : Model -> Animator.Animation
animation model =
    let
        timeline =
            if model.scenario == "gc-delayed" then
                Timeline.delay (Animator.ms 5000) model.timeline

            else
                model.timeline

        steps =
            [ Animator.set (opacity 0)
            , Animator.step (Animator.ms 1000) (opacity 1)
            ]
    in
    case model.scenario of
        "finite-loop" ->
            Animator.keyframes [ Animator.loopFor 2 steps ]

        "infinite-loop" ->
            Animator.keyframes [ Animator.loop steps ]

        "nested-repeat" ->
            Animator.keyframes [ Animator.loopFor 2 [ Animator.sequence [ Animator.loopFor 3 steps ] ] ]

        "nested" ->
            Animator.keyframes
                [ Animator.set (opacity 0)
                , Animator.sequence
                    [ Animator.loopFor 2 [ Animator.step (Animator.ms 1000) (opacity 1) ]
                    , Animator.wait (Animator.ms 500)
                    , Animator.step (Animator.ms 500) (opacity 0)
                    ]
                ]

        "instant-reset" ->
            Animator.keyframes
                [ Animator.loop
                    [ Animator.set (opacity 0)
                    , Animator.step (Animator.ms 1000) (opacity 1)
                    , Animator.set (opacity 0)
                    , Animator.step (Animator.ms 1000) (opacity 1)
                    ]
                ]

        "native" ->
            Animator.transition (Animator.ms 1000) (opacity model.target)

        "spring" ->
            Animator.transition (Animator.ms 1000)
                [ Animator.x (model.target * 100) |> Animator.withTransition (Transition.spring { wobble = 1, quickness = 0 }) ]

        "resting" ->
            Animator.onTimelineWith model.timeline
                (\attrs ->
                    ( attrs
                    , [ Animator.loop
                            [ Animator.set (opacity 1)
                            , Animator.step (Animator.ms 1000) (opacity 0)
                            , Animator.step (Animator.ms 1000) (opacity 1)
                            ]
                      ]
                    )
                )

        _ ->
            if model.renderer == "onTimeline" then
                Animator.onTimeline timeline identity

            else
                Animator.onTimelineWith timeline (\attrs -> ( attrs, [] ))
