module Simple exposing (main)

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



{- Normal Timelines -}


singleEvent =
    Animator.init Hufflepuff
        |> Internal.Timeline.updateWith False (Time.millisToPosix 0)


doubleEvent =
    singleEvent
        |> Animator.queue
            [ Animator.wait (Animator.seconds 1)
            , Animator.transitionTo (Animator.millis 250) Griffyndor
            ]
        |> Internal.Timeline.updateWith False (Time.millisToPosix 0)


main =
    Browser.document
        { init =
            \() ->
                update QueueOne
                    { timeline =
                        Animator.init Hufflepuff
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
    let
        _ =
            Interpolate.details
                (doubleEvent
                    |> Internal.Timeline.updateWith False (Time.millisToPosix 1002)
                 -- |> Debug.log "timeline"
                )
                (toHousePosition >> Interpolate.withStandardDefault)

        -- |> Debug.log "******** end pos/osc"
        -- _ =
        --     Animator.details fourContinuous oscillators
        --         |> Debug.log "******* end osc "
    in
    { title = "Elm - Select Harry Potter House"
    , body =
        [-- viewTimelineGroup "Single Event"
         -- [ { name = "Pos"
         --   , timeline = singleEvent
         --   , move = toHousePosition
         --   }
         -- , { name = "Oscillator"
         --   , timeline = singleEvent
         --   , move = oscillators
         --   }
         -- , { name = "Pos -> Oscillators"
         --   , timeline = singleEvent
         --   , move = posThenOscillators
         --   }
         -- , { name = "Sorta Wobbly"
         --   , timeline = singleEvent
         --   , move = sortaWobbly
         --   }
         -- , { name = "Wobbly"
         --   , timeline = singleEvent
         --   , move = wobbly
         --   }
         -- ]
         -- ,
         {--}
         --   viewTimelineGroup "Double Event"
         --     [ { name = "Pos"
         --       , timeline = doubleEvent
         --       , move = toHousePosition
         --       }
         --     ]
         {--}
         -- , { name = "Oscillator"
         --   , timeline = doubleEvent
         --   , move = oscillators
         --   }
         -- , { name = "Pos -> Oscillators"
         --   , timeline = doubleEvent
         --   , move = posThenOscillators
         --   }
         -- , { name = "Sorta wobbly"
         --   , timeline = doubleEvent
         --   , move = sortaWobbly
         --   }
         -- , { name = "Wobbly"
         --   , timeline = doubleEvent
         --   , move = wobbly
         --   }
         -- ]
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
            case move (Internal.Timeline.update currentTime timeline) toPos of
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
            case move (Internal.Timeline.update currentTime timeline) toPos of
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
                        :: Animator.transitionTo (Animator.seconds 1) (next house)
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
                        [ Animator.transitionTo (Animator.seconds 1) Slytherin
                        , Animator.wait (Animator.seconds 3)
                        ]
                        model.timeline
                        |> Internal.Timeline.update (Time.millisToPosix 0)
                        |> Internal.Timeline.update (Time.millisToPosix 1409)
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
                    Animator.queue [ Animator.transitionTo (Animator.seconds 1) newHouse ] model.timeline
                , house = newHouse
              }
            , Cmd.none
            )

        NewTime newPosix ->
            ( { model
                | time = newPosix
                , timeline =
                    Internal.Timeline.update newPosix model.timeline
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
            Animator.at 100

        Griffyndor ->
            Animator.at 400

        Slytherin ->
            Animator.at 700

        Ravenclaw ->
            Animator.at 1000


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
            Animator.at 100

        Griffyndor ->
            Animator.wave 390 410
                |> Animator.loop (Animator.millis 300)

        Slytherin ->
            Animator.at 700

        Ravenclaw ->
            Animator.wave 990 1010
                |> Animator.loop (Animator.millis 500)


toHousePositionFastStartSlowFinish event =
    case event of
        Hufflepuff ->
            Animator.at 100
                |> Animator.leaveSmoothly 0.4

        Griffyndor ->
            Animator.at 400

        Slytherin ->
            Animator.at 700
                |> Animator.arriveSmoothly 0.8

        Ravenclaw ->
            Animator.at 1000


toHousePositionWithOrbit : House -> Animator.Movement
toHousePositionWithOrbit event =
    case event of
        Hufflepuff ->
            Animator.at 100

        Griffyndor ->
            Animator.wave 390 410
                |> Animator.loop (Animator.millis 400)

        Slytherin ->
            Animator.at 700

        Ravenclaw ->
            Animator.at 1000


wobbly event =
    case event of
        Hufflepuff ->
            Animator.at 100
                |> Animator.withWobble 1

        Griffyndor ->
            Animator.at 400
                |> Animator.withWobble 1

        Slytherin ->
            Animator.at 700
                |> Animator.withWobble 1

        Ravenclaw ->
            Animator.at 1000
                |> Animator.withWobble 1


mixing event =
    case event of
        Hufflepuff ->
            Animator.wave 90 110
                |> Animator.loop (Animator.millis 200)

        Griffyndor ->
            Animator.wave 90 110
                |> Animator.loop (Animator.millis 300)

        Slytherin ->
            Animator.wave 90 110
                |> Animator.loop (Animator.millis 400)

        Ravenclaw ->
            Animator.wave 90 110
                |> Animator.loop (Animator.millis 500)


sortaWobbly event =
    case event of
        Hufflepuff ->
            Animator.at 100
                |> Animator.withWobble 0.5

        Griffyndor ->
            Animator.at 400
                |> Animator.withWobble 0.5

        Slytherin ->
            Animator.at 700
                |> Animator.withWobble 0.5

        Ravenclaw ->
            Animator.at 1000
                |> Animator.withWobble 0.5
