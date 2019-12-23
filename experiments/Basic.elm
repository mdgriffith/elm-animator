module Basic exposing (main, subscriptions, update, view)

import Animator
import Browser
import Color
import Ease
import Help.Plot
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Internal.Timeline
import Time


main =
    Browser.document
        { init =
            \() ->
                update QueueOne
                    { timeline =
                        Animator.init (Time.millisToPosix 0) Hufflepuff
                    , time = Time.millisToPosix 0
                    , house = Hufflepuff
                    , chart = Help.Plot.init
                    }

        -- , Cmd.none
        -- )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { timeline : Animator.Timeline House
    , time : Time.Posix
    , house : House
    , chart : Help.Plot.Model
    }


type Msg
    = Tick (Animator.Timeline House)
    | NextHouse
    | QueueThree
    | QueueOne
    | NewTime Time.Posix
    | ChartMsg Help.Plot.Msg


type House
    = Hufflepuff
    | Griffyndor
    | Slytherin
    | Ravenclaw


row attrs =
    Html.div
        ([ Attr.style "display" "flex"
         , Attr.style "flex-direction" "row"
         , Attr.style "align-items" "center"
         ]
            ++ attrs
        )


view : Model -> Browser.Document Msg
view model =
    { title = "Elm - Select Harry Potter House"
    , body =
        [ row []
            [ -- Help.Plot.timeline
              -- { timeline = model.timeline
              -- , toMovement = toHousePosition
              -- }
              Help.Plot.timeline
                { timeline = model.timeline
                , toMovement = toHousePositionFastStartSlowFinish
                }

            -- , Help.Plot.timeline
            --     { timeline = model.timeline
            --     , toMovement = toHousePosition
            --     }
            ]

        -- , row []
        --     [ Help.Plot.easing Ease.linear 0 1
        --     ]
        -- , row []
        --     [ Help.Plot.easing Ease.inQuad 0 1
        --     , Help.Plot.easing Ease.outQuad 0 1
        --     , Help.Plot.easing Ease.inOutQuad 0 1
        --     ]
        -- , row []
        --     [ Help.Plot.easing Ease.inCubic 0 1
        --     , Help.Plot.easing Ease.outCubic 0 1
        --     , Help.Plot.easing Ease.inOutCubic 0 1
        --     ]
        -- , row []
        --     [ Help.Plot.easing Ease.inQuart 0 1
        --     , Help.Plot.easing Ease.outQuart 0 1
        --     , Help.Plot.easing Ease.inOutQuart 0 1
        --     ]
        -- , row []
        --     [ Help.Plot.easing Ease.inQuint 0 1
        --     , Help.Plot.easing Ease.outQuint 0 1
        --     , Help.Plot.easing Ease.inOutQuint 0 1
        --     ]
        -- , row []
        --     [ Help.Plot.easing Ease.inSine 0 1
        --     , Help.Plot.easing Ease.outSine 0 1
        --     , Help.Plot.easing Ease.inOutSine 0 1
        --     ]
        -- , row []
        --     [ Help.Plot.easing Ease.inExpo 0 1
        --     , Help.Plot.easing Ease.outExpo 0 1
        --     , Help.Plot.easing Ease.inOutExpo 0 1
        --     ]
        -- , row []
        --     [ Help.Plot.easing Ease.inCirc 0 1
        --     , Help.Plot.easing Ease.outCirc 0 1
        --     , Help.Plot.easing Ease.inOutCirc 0 1
        --     ]
        -- , row []
        --     [ Help.Plot.easing Ease.inBack 0 1
        --     , Help.Plot.easing Ease.outBack 0 1
        --     , Help.Plot.easing Ease.inOutBack 0 1
        --     ]
        -- , row []
        --     [ Help.Plot.easing Ease.inElastic 0 1
        --     , Help.Plot.easing Ease.outElastic 0 1
        --     , Help.Plot.easing Ease.inOutElastic 0 1
        --     ]
        -- , row []
        --     [ Help.Plot.easing sin 0 (2 * pi) --(\i -> sin (turns i))
        --     , Help.Plot.easing (\i -> sin (turns i)) 0 1
        --     ]
        -- -- noWobble
        -- , Html.span [] [ Html.text "noWobble" ]
        -- , Html.div [ Attr.style "display" "flex", Attr.style "direction" "flex-row" ]
        --     [ Help.Plot.spring
        --         { stiffness = 170
        --         , damping = 26
        --         , initialPosition = 0
        --         , initialVelocity = 0
        --         }
        --     , Help.Plot.spring
        --         { stiffness = 170
        --         , damping = 26
        --         , initialPosition = -10
        --         , initialVelocity = 0
        --         }
        --     , Help.Plot.spring
        --         { stiffness = 170
        --         , damping = 26
        --         , initialPosition = 0
        --         , initialVelocity = 500
        --         }
        --     ]
        -- -- stiff
        -- , Html.span [] [ Html.text "stiff" ]
        -- , Html.div [ Attr.style "display" "flex", Attr.style "direction" "flex-row" ]
        --     [ Help.Plot.spring
        --         { stiffness = 210
        --         , damping = 20
        --         , initialPosition = 0
        --         , initialVelocity = 0
        --         }
        --     , Help.Plot.spring
        --         { stiffness = 210
        --         , damping = 20
        --         , initialPosition = -10
        --         , initialVelocity = 0
        --         }
        --     , Help.Plot.spring
        --         { stiffness = 210
        --         , damping = 20
        --         , initialPosition = 0
        --         , initialVelocity = 500
        --         }
        --     ]
        -- -- gentle
        -- , Html.span [] [ Html.text "gentle" ]
        -- , Html.div [ Attr.style "display" "flex", Attr.style "direction" "flex-row" ]
        --     [ Help.Plot.spring
        --         { stiffness = 120
        --         , damping = 14
        --         , initialPosition = 0
        --         , initialVelocity = 0
        --         }
        --     , Help.Plot.spring
        --         { stiffness = 120
        --         , damping = 14
        --         , initialPosition = -10
        --         , initialVelocity = 0
        --         }
        --     , Help.Plot.spring
        --         { stiffness = 120
        --         , damping = 14
        --         , initialPosition = 0
        --         , initialVelocity = 500
        --         }
        --     ]
        -- -- wobbly
        -- , Html.span [] [ Html.text "wobbly" ]
        -- , Html.div [ Attr.style "display" "flex", Attr.style "direction" "flex-row" ]
        --     [ Help.Plot.spring
        --         { stiffness = 180
        --         , damping = 12
        --         , initialPosition = 0
        --         , initialVelocity = 0
        --         }
        --     , Help.Plot.spring
        --         { stiffness = 180
        --         , damping = 12
        --         , initialPosition = -10
        --         , initialVelocity = 0
        --         }
        --     , Help.Plot.spring
        --         { stiffness = 180
        --         , damping = 12
        --         , initialPosition = 0
        --         , initialVelocity = 500
        --         }
        --     ]
        -- , Html.span [] [ Html.text "Critical Damping vs K" ]
        -- , Html.div [ Attr.style "display" "flex", Attr.style "direction" "flex-row" ]
        --     [ Help.Plot.damping
        --         { kMin = 10
        --         , kMax = 300
        --         }
        --     ]
        -- , Html.span [] [ Html.text "Wobles, K vs Settling Time" ]
        -- , Html.div [ Attr.style "display" "flex", Attr.style "direction" "flex-row" ]
        --     [ Help.Plot.settlingTime
        --         { kMin = 10
        --         , kMax = 600
        --         , wobbles =
        --             [ 0
        --             , 0.1
        --             , 0.25
        --             , 0.5
        --             , 75
        --             , 1
        --             ]
        --         }
        --     ]
        ]
    }


renderEvents events =
    List.map
        (\( time, ev ) ->
            { time = toFloat (Time.posixToMillis time)
            , position = 300
            }
        )
        events


renderPoints move timeline toPos =
    List.foldl
        (\i rendered ->
            let
                currentTime =
                    Time.millisToPosix (i * 16)
            in
            case move (Animator.update currentTime timeline) toPos of
                current ->
                    { time = toFloat i * 16
                    , position = current.position
                    }
                        :: rendered
        )
        []
        (List.range 0 400)


renderVelocities move timeline toPos =
    List.foldl
        (\i rendered ->
            let
                currentTime =
                    Time.millisToPosix (i * 16)
            in
            case move (Animator.update currentTime timeline) toPos of
                current ->
                    { time = toFloat i * 16
                    , position = current.velocity
                    }
                        :: rendered
        )
        []
        (List.range 0 400)


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

        ChartMsg chartMsg ->
            ( { model | chart = Help.Plot.update chartMsg model.chart }
            , Cmd.none
            )

        QueueThree ->
            let
                addToQueue _ ( q, house ) =
                    ( Animator.wait (Animator.seconds 1)
                        :: Animator.event (Animator.seconds 1) (next house)
                        :: q
                    , next house
                    )

                ( forQueue, newHouse ) =
                    List.foldl addToQueue ( [], model.house ) (List.range 1 3)
            in
            ( { model
                | timeline =
                    Animator.queue (Animator.wait (Animator.seconds 0.5) :: List.reverse forQueue) model.timeline
              }
            , Cmd.none
            )

        QueueOne ->
            ( { model
                | timeline =
                    Animator.queue
                        [ Animator.event (Animator.seconds 1) Slytherin
                        , Animator.wait (Animator.seconds 3)
                        ]
                        model.timeline
                        |> Animator.update (Time.millisToPosix 0)
                        |> Animator.update (Time.millisToPosix 1409)
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


toHousePositionFastStartSlowFinish event =
    case event of
        Hufflepuff ->
            Animator.to 100
                |> Animator.leave Animator.slowly

        Griffyndor ->
            Animator.to 400

        Slytherin ->
            Animator.to 700
                |> Animator.arrive Animator.verySlowly

        Ravenclaw ->
            Animator.to 1000


toHousePositionWithOrbit : House -> Animator.Movement
toHousePositionWithOrbit event =
    case event of
        Hufflepuff ->
            Animator.to 100

        Griffyndor ->
            Animator.wave 390 410
                |> Animator.oscillate (Animator.millis 400)

        Slytherin ->
            Animator.to 700

        Ravenclaw ->
            Animator.to 1000
