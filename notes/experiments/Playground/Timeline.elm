module Playground.Timeline exposing (main)

{- Timeline Playground!


   This is for getting a visual representation of a timeline.


       1. Plot all events on a symbolic timeline
           - Plot unvisitable events as well, just have them be a separate style.
           - Only event names as labels
           - More data as tooltips

       2. Scrub through timeline
           - Set the current time of the timeline without updating it.
               - can't go before last hard update
           - See current, prev, upcoming, arrived vals
           - See generated CSS
           - See what the timeline would be if we updated


   ----

       3. See actual rendered values on the timeline
           - Plot the bezier splines
           - Plot the actual values at regular intervals


-}

import Animator
import Animator.Css2
import Browser
import Duration
import Html exposing (Html, button, div, h1, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Internal.Bezier as Bezier
import Internal.Css as Css
import Internal.Css.Props
import Internal.Interpolate as Interpolate
import Internal.Spring as Spring
import Internal.Time as Time
import Internal.Timeline as Timeline
import Pixels
import Playground.View.Timeline
import Svg
import Svg.Attributes as SvgA
import Time


main =
    Browser.document
        { init =
            \() ->
                ( { -- timeline =
                    -- Animator.init (State 0)
                    --     |> Animator.queue
                    --         [ Animator.transitionTo (Animator.millis 100) (State 1)
                    --         , Animator.transitionTo (Animator.millis 100) (State 2)
                    --         , Animator.transitionTo (Animator.millis 100) (State 3)
                    --         ]
                    --     |> Timeline.update (Time.millisToPosix 0)
                    --     |> Animator.interrupt
                    --         [ Animator.transitionTo (Animator.millis 100) (State 4)
                    --         , Animator.transitionTo (Animator.millis 100) (State 5)
                    --         , Animator.transitionTo (Animator.millis 100) (State 6)
                    --         ]
                    --     |> Timeline.update (Time.millisToPosix 150)
                    --   ,
                    lastUpdated = Time.millisToPosix 0
                  , tooltip = Nothing
                  , levels =
                        createLevels
                            { events = 3
                            , depth = 2
                            }

                  -- |> Debug.log "levels"
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions =
            \_ -> Sub.none
        }


createLevels :
    { depth : Int, events : Int }
    ->
        List
            { start : Int
            , events : List (Animator.Step State)
            }
createLevels details =
    createLevelsHelper details.depth details []


createLevelsHelper index details created =
    if index == 0 then
        created

    else
        createLevelsHelper
            (index - 1)
            details
            ({ start = (index * 100) + 50
             , events =
                let
                    base =
                        ((index - 1) * details.events) + 1
                in
                List.range base (base + details.events - 1)
                    |> List.map
                        (\i ->
                            Animator.transitionTo (Animator.millis 100) (State i)
                        )
             }
                :: created
            )


type alias Model =
    { timeline : Animator.Timeline State
    , lastUpdated : Time.Posix
    , tooltip : Maybe Tooltip

    -- interactive generation of timeline
    , levels :
        List
            { start : Int
            , events : List (Animator.Step State)
            }
    }


type alias Tooltip =
    { anchor : Interpolate.Point
    , text : List ( String, String )
    }


type State
    = State Int


type Msg
    = ScrubTo Time.Posix
    | TooltipShow Tooltip
    | TooltipClear


update msg model =
    case msg of
        ScrubTo time ->
            ( model, Cmd.none )

        TooltipShow tooltip ->
            ( { model | tooltip = Just tooltip }, Cmd.none )

        TooltipClear ->
            ( { model | tooltip = Nothing }, Cmd.none )


type Style
    = Highlight
    | Normal
    | Faded
    | Mini


view model =
    { title = "Timeline Playground"
    , body = [ viewBody model ]
    }


createTimeline levels tl =
    case levels of
        [] ->
            tl

        top :: remain ->
            createTimeline remain
                (tl
                    |> Animator.interrupt top.events
                    |> Timeline.update (Time.millisToPosix top.start)
                )


startingTimeline : Timeline.Timeline State
startingTimeline =
    Animator.init (State 0)
        |> Timeline.update (Time.millisToPosix 0)


viewBody model =
    let
        timeline =
            createTimeline model.levels startingTimeline
                |> Timeline.update (Time.millisToPosix 300)
    in
    div []
        [ h1 [] [ text "Timeline Playground" ]
        , case model.tooltip of
            Nothing ->
                Html.text ""

            Just tooltip ->
                Html.div
                    [ Attr.style "position" "absolute"
                    , Attr.style "right" "100px"
                    , Attr.style "top" "100px"
                    ]
                    (List.map
                        (\( name, val ) ->
                            Html.div []
                                [ Html.text name
                                , Html.text ": "
                                , Html.text val
                                ]
                        )
                        tooltip.text
                    )
        , Playground.View.Timeline.view timeline
            { values = True
            , splines = True
            , toValues = toValues
            }
        , Playground.View.Timeline.viewTimeline timeline

        -- , Playground.View.Timeline.viewCss toValues timeline
        , Playground.View.Timeline.viewCssProps toProps timeline
        ]


toProps : State -> List Css.Prop
toProps (State state) =
    let
        base =
            toFloat state * 100
    in
    -- Interpolate.Pos Interpolate.standardDefault (toFloat (state * 100))
    [ -- Css.Prop Internal.Css.Props.ids.opacity
      -- (wave (Timeline.Repeat 5 (Animator.millis 200)) base (base + 100))
      Animator.Css2.opacity
        (Animator.at (base / 500))
    , Animator.Css2.rotation
        (Animator.at 0)
    , Animator.Css2.x
        (Animator.at base)
    ]


toValues : State -> Interpolate.Movement
toValues (State state) =
    let
        base =
            toFloat state * 100
    in
    -- Interpolate.Pos Interpolate.standardDefault (toFloat (state * 100))
    wave (Timeline.Repeat 5 (Animator.millis 200)) base (base + 100)



{- OSCILLATORS -}


at : Float -> Interpolate.Movement
at state =
    Interpolate.Pos Interpolate.standardDefault (state * 100)


wave : Timeline.Period -> Float -> Float -> Interpolate.Movement
wave period start end =
    let
        top =
            max start end

        bottom =
            min start end

        total =
            top - bottom

        periodDuration =
            case period of
                Timeline.Loop dur ->
                    dur

                Timeline.Repeat n dur ->
                    dur
    in
    Interpolate.Osc Interpolate.standardDefault
        start
        period
        -- TODO! What are the bezier control points for a sin wave?
        -- (\u ->
        --     let
        --         normalized =
        --             (cos (turns (0.5 + u)) + 1) / 2
        --     in
        --     start + total * normalized
        -- )
        (scaleCurveTiming 0
            periodDuration
            []
            [ { value = end
              , timing =
                    Interpolate.Bezier
                        (Bezier.Spline
                            { x = 0
                            , y = start
                            }
                            { x = 0
                            , y = start
                            }
                            { x = 0.5
                            , y = end
                            }
                            { x = 0.5
                            , y = end
                            }
                        )

              -- Interpolate.Linear
              , time = 0.5
              }
            , { value = start
              , timing =
                    Interpolate.Bezier
                        (Bezier.Spline
                            { x = 0.5
                            , y = end
                            }
                            { x = 0.5
                            , y = end
                            }
                            { x = 1
                            , y = start
                            }
                            { x = 1
                            , y = start
                            }
                        )

              -- Interpolate.Linear
              , time = 1
              }
            ]
        )


scaleCurveTiming last periodDuration rendered sections =
    case sections of
        [] ->
            List.reverse rendered

        -- |> Debug.log "dwelll curves"
        point :: remain ->
            scaleCurveTiming
                point.time
                periodDuration
                (scaleTiming last periodDuration point :: rendered)
                remain


scaleTiming last periodDuration point =
    case point.timing of
        Interpolate.Linear ->
            point

        Interpolate.Bezier (Bezier.Spline c0 c1 c2 c3) ->
            let
                -- _ = Debug.log "SCALING" (duration, point.time, last)
                duration =
                    --  (point.time - last) *
                    Duration.inMilliseconds periodDuration

                -- current points take the form of
                -- x : 0-1
                -- y : actual position
                -- and we want to scale x to be real duration numbers in millis
                sc0 =
                    { x = c0.x * duration
                    , y = c0.y
                    }

                sc1 =
                    { x = c1.x * duration
                    , y = c1.y
                    }

                sc2 =
                    { x = c2.x * duration
                    , y = c2.y
                    }

                sc3 =
                    { x = c3.x * duration
                    , y = c3.y
                    }
            in
            { value = point.value
            , timing =
                Interpolate.Bezier
                    (Bezier.Spline sc0 sc1 sc2 sc3)
            , time = point.time
            }


{-| Start at one number, move linearly to another, and then linearly back.
-}
zigzag : Timeline.Period -> Float -> Float -> Interpolate.Movement
zigzag period start end =
    let
        total =
            end - start
    in
    Interpolate.Osc Interpolate.standardDefault
        start
        period
        [ { value = end
          , timing = Interpolate.Linear
          , time = 0.5
          }
        , { value = start
          , timing = Interpolate.Linear
          , time = 1
          }
        ]


{-| Start at one number and move linearly to another, then immediately start again at the first.

This was originally intended for animating rotation where you'd want 360deg to "wrap" to 0deg.

-}
wrap : Timeline.Period -> Float -> Float -> Interpolate.Movement
wrap period start end =
    let
        total =
            end - start
    in
    Interpolate.Osc Interpolate.standardDefault
        start
        period
        [ { value = end
          , timing = Interpolate.Linear
          , time = 1
          }
        ]
