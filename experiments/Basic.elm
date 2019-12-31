module Basic exposing (main, subscriptions, update, view)

import Animator
import Browser
import Color
import Duration
import Ease
import Help.Plot
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Internal.Interpolate as Interpolate
import Internal.Spring as Spring
import Internal.Timeline
import Time


singleEvent =
    Animator.init (Time.millisToPosix 0) Hufflepuff


doubleEvent =
    singleEvent
        |> Animator.queue
            [ Animator.wait (Animator.seconds 1)
            , Animator.event (Animator.seconds 1) Griffyndor
            ]
        |> Animator.update (Time.millisToPosix 0)
        |> Animator.update (Time.millisToPosix 1200)


fourContinuous =
    singleEvent
        |> Animator.queue
            [ Animator.event (Animator.seconds 1) Griffyndor
            , Animator.event (Animator.seconds 1) Slytherin
            , Animator.event (Animator.seconds 1) Ravenclaw
            ]
        |> Animator.update (Time.millisToPosix 0)
        |> Animator.update (Time.millisToPosix 1200)


fourWithPause =
    singleEvent
        |> Animator.queue
            [ Animator.wait (Animator.seconds 1)
            , Animator.event (Animator.seconds 1) Griffyndor
            , Animator.wait (Animator.seconds 1)
            , Animator.event (Animator.seconds 1) Slytherin
            , Animator.wait (Animator.seconds 1)
            , Animator.event (Animator.seconds 1) Ravenclaw
            , Animator.wait (Animator.seconds 1)
            ]
        |> Animator.update (Time.millisToPosix 0)
        |> Animator.update (Time.millisToPosix 3409)


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


column attrs =
    Html.div
        ([ Attr.style "display" "flex"
         , Attr.style "flex-direction" "column"
         , Attr.style "align-items" "center"
         ]
            ++ attrs
        )


viewTimelineGroup title timelines =
    column
        []
        [ Html.h2 [] [ Html.text title ]
        , row [] (List.map viewTimeline timelines)
        ]


viewTimeline { name, timeline, move } =
    column []
        [ Html.h3 [] [ Html.text name ]
        , Help.Plot.timeline
            { timeline = timeline
            , toMovement = move
            }
        ]


view : Model -> Browser.Document Msg
view model =
    -- let
    --     _ =
    --         Animator.move doubleEvent sortaWobbly
    --             |> Debug.log "******** end pos/osc"
    --     -- _ =
    --     --     Animator.move fourContinuous oscillators
    --     --         |> Debug.log "******* end osc "
    -- in
    { title = "Elm - Select Harry Potter House"
    , body =
        [ viewTimelineGroup "Single Event"
            [ { name = "Pos"
              , timeline = singleEvent
              , move = toHousePosition
              }
            , { name = "Oscillator"
              , timeline = singleEvent
              , move = oscillators
              }
            , { name = "Pos -> Oscillators"
              , timeline = singleEvent
              , move = posThenOscillators
              }
            , { name = "Sorta Wobbly"
              , timeline = singleEvent
              , move = sortaWobbly
              }
            , { name = "Wobbly"
              , timeline = singleEvent
              , move = wobbly
              }
            ]
        , viewTimelineGroup "Double Event"
            [ { name = "Pos"
              , timeline = doubleEvent
              , move = toHousePosition
              }
            , { name = "Oscillator"
              , timeline = doubleEvent
              , move = oscillators
              }
            , { name = "Pos -> Oscillators"
              , timeline = doubleEvent
              , move = posThenOscillators
              }
            , { name = "Sorta wobbly"
              , timeline = doubleEvent
              , move = sortaWobbly
              }
            , { name = "Wobbly"
              , timeline = doubleEvent
              , move = wobbly
              }
            ]
        , viewTimelineGroup "Four Continuous"
            [ { name = "Pos"
              , timeline = fourContinuous
              , move = toHousePosition
              }
            , { name = "Oscillator"
              , timeline = fourContinuous
              , move = oscillators
              }
            , { name = "Pos -> Oscillators"
              , timeline = fourContinuous
              , move = posThenOscillators
              }
            , { name = "Sorta Wobbly"
              , timeline = fourContinuous
              , move = sortaWobbly
              }
            , { name = "Wobbly"
              , timeline = fourContinuous
              , move = wobbly
              }
            ]
        , viewTimelineGroup "Four with Pause"
            [ { name = "Pos"
              , timeline = fourWithPause
              , move = toHousePosition
              }
            , { name = "Oscillator"
              , timeline = fourWithPause
              , move = oscillators
              }
            , { name = "Pos -> Oscillators"
              , timeline = fourWithPause
              , move = posThenOscillators
              }
            , { name = "Sorta Wobbly"
              , timeline = fourWithPause
              , move = sortaWobbly
              }
            , { name = "Wobbly"
              , timeline = fourWithPause
              , move = wobbly
              }
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
        --     noWobble
        , viewSpringVariations "noWobble"
            { stiffness = 170
            , damping = 26
            , mass = 1
            }

        -- stiff
        , viewSpringVariations "stiff"
            { stiffness = 210
            , damping = 20
            , mass = 1
            }

        -- gentle
        , viewSpringVariations "gentle"
            { stiffness = 120
            , damping = 14
            , mass = 1
            }

        -- wobbly
        , viewSpringVariations "wobbly"
            { stiffness = 180
            , damping = 12
            , mass = 1
            }

        -- standard stiffness
        , viewSpringVariations "standard stiffness, no wobble - 10"
            { stiffness = 150
            , damping = 10
            , mass = 1
            }

        -- standard stiffness
        , viewSpringVariations "standard stiffness, no wobble - 12"
            { stiffness = 150
            , damping = 12
            , mass = 1
            }

        -- standard stiffness
        , viewSpringVariations "standard stiffness, no wobble - 15"
            { stiffness = 150
            , damping = 15
            , mass = 1
            }

        -- standard stiffness
        , viewSpringVariations "standard stiffness, no wobble - 19"
            { stiffness = 150
            , damping = 19
            , mass = 1
            }
        , viewSpringVariations "standard stiffness, full wobble"
            { stiffness = 150
            , damping = 22
            , mass = 1
            }
        , let
            mostWobble =
                Spring.select 1 (Duration.seconds 0.75)
          in
          viewSpringVariations "Selected Spring, full wobble"
            mostWobble
        , let
            leastWobble =
                Spring.select 0 (Duration.seconds 0.75)
          in
          viewSpringVariations "Selected Spring, no wobble, short duration"
            leastWobble
        , let
            -- _ =
            --     Debug.log "critical dampings"
            --         { base = Spring.criticalDamping 150 1
            --         , high = Spring.criticalDamping 150 1.5
            --         , low = Spring.criticalDamping 150 0.5
            --         }
            leastWobble =
                Spring.select 0 (Duration.seconds 0.25)
          in
          viewSpringVariations "Selected Spring, most wobble"
            leastWobble
        , Html.span [] [ Html.text "Critical Damping vs K" ]
        , Html.div [ Attr.style "display" "flex", Attr.style "direction" "flex-row" ]
            [ Help.Plot.damping
                { kMin = 10
                , kMax = 300
                }
            ]
        , Html.span [] [ Html.text "Wobles, K vs Settling Time" ]
        , Html.div [ Attr.style "display" "flex", Attr.style "direction" "flex-row" ]
            [ Help.Plot.settlingTime
                { kMin = 120
                , kMax = 210
                , wobbles =
                    [ 0
                    , 0.1
                    , 0.25
                    , 0.5
                    , 75
                    , 1
                    ]
                }
            ]
        ]
    }


viewSpringVariations label params =
    Html.div []
        [ Html.span [] [ Html.text label ]
        , Html.div [ Attr.style "display" "flex", Attr.style "direction" "flex-row" ]
            [ Help.Plot.spring
                { stiffness = params.stiffness
                , damping = params.damping
                , mass = params.mass
                , initialPosition = 0
                , initialVelocity = 0
                }
            , Help.Plot.spring
                { stiffness = params.stiffness
                , damping = params.damping
                , mass = params.mass
                , initialPosition = -10
                , initialVelocity = 0
                }
            , Help.Plot.spring
                { stiffness = params.stiffness
                , damping = params.damping
                , mass = params.mass
                , initialPosition = 0
                , initialVelocity = 500
                }
            ]
        ]


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


oscillators event =
    case event of
        Hufflepuff ->
            Animator.wave 90 110
                |> Animator.loop (Animator.millis 200)

        Griffyndor ->
            Animator.wave 390 410
                |> Animator.loop (Animator.millis 300)

        Slytherin ->
            Animator.wave 690 710
                |> Animator.loop (Animator.millis 400)

        Ravenclaw ->
            Animator.wave 990 1010
                |> Animator.loop (Animator.millis 500)


posThenOscillators event =
    case event of
        Hufflepuff ->
            Animator.to 100

        Griffyndor ->
            Animator.wave 390 410
                |> Animator.loop (Animator.millis 300)

        Slytherin ->
            Animator.to 700

        Ravenclaw ->
            Animator.wave 990 1010
                |> Animator.loop (Animator.millis 500)


toHousePositionFastStartSlowFinish event =
    case event of
        Hufflepuff ->
            Animator.to 100
                |> Animator.leave Animator.smooth

        Griffyndor ->
            Animator.to 400

        Slytherin ->
            Animator.to 700
                |> Animator.arrive Animator.verySmooth

        Ravenclaw ->
            Animator.to 1000


toHousePositionWithOrbit : House -> Animator.Movement
toHousePositionWithOrbit event =
    case event of
        Hufflepuff ->
            Animator.to 100

        Griffyndor ->
            Animator.wave 390 410
                |> Animator.loop (Animator.millis 400)

        Slytherin ->
            Animator.to 700

        Ravenclaw ->
            Animator.to 1000


wobbly event =
    case event of
        Hufflepuff ->
            Animator.to 100
                |> Animator.withWobble 1

        Griffyndor ->
            Animator.to 400
                |> Animator.withWobble 1

        Slytherin ->
            Animator.to 700
                |> Animator.withWobble 1

        Ravenclaw ->
            Animator.to 1000
                |> Animator.withWobble 1


sortaWobbly event =
    case event of
        Hufflepuff ->
            Animator.to 100
                |> Animator.withWobble 0.5

        Griffyndor ->
            Animator.to 400
                |> Animator.withWobble 0.5

        Slytherin ->
            Animator.to 700
                |> Animator.withWobble 0.5

        Ravenclaw ->
            Animator.to 1000
                |> Animator.withWobble 0.5
