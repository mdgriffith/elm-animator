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

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Svg
import Svg.Attributes as SvgA
import Internal.Spring as Spring
import Internal.Timeline as Timeline
import Duration
import Internal.Interpolate as Interpolate
import Internal.Bezier as Bezier
import Internal.Css as Css
import Animator
import Time
import Internal.Time as Time
import Pixels
import Playground.View.Timeline

main =
    Browser.document
        { init =
            \() ->
                ( { 
                    
                    -- timeline = 
                    -- Animator.init (State 0)
                    --     |> Animator.queue
                    --         [ Animator.event (Animator.millis 100) (State 1)
                    --         , Animator.event (Animator.millis 100) (State 2)
                    --         , Animator.event (Animator.millis 100) (State 3)
                    --         ]
                    --     |> Timeline.update (Time.millisToPosix 0)
                    --     |> Animator.interrupt 
                    --         [ Animator.event (Animator.millis 100) (State 4)
                    --         , Animator.event (Animator.millis 100) (State 5)
                    --         , Animator.event (Animator.millis 100) (State 6)
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
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = 
            \_ -> Sub.none
        }


createLevels : {depth : Int, events : Int} ->  List 
            { start : Int
            , events : List (Animator.Step State)
            }
createLevels details =
    createLevelsHelper details.depth details  []


createLevelsHelper index details created =
    if index == 0 then
        created
    else
        createLevelsHelper
            (index - 1)
            details
            ({ start = index * 150
             , events =
                let
                    base = 
                        ((index - 1) * details.events) + 1
                in
                List.range base (base + details.events - 1)
                    |> List.map 
                        (\i ->
                            Animator.event (Animator.millis 100) (State i)
                        )
            } :: created

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
    , text : List (String, String)
    }


type State = State Int

type Msg 
    = ScrubTo Time.Posix
    | TooltipShow Tooltip
    | TooltipClear



update msg model =
    case msg of
        ScrubTo time ->
            (model, Cmd.none)

        TooltipShow tooltip ->
            ({ model | tooltip = Just tooltip },Cmd.none)

        TooltipClear ->
            ({ model | tooltip = Nothing },Cmd.none)



type Style = Highlight | Normal | Faded | Mini


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
                ( tl
                    |> Animator.interrupt top.events 
                    |> Timeline.update (Time.millisToPosix top.start)
                )


viewBody model =
    let
        timeline = 
            createTimeline model.levels (Animator.init (State 0))
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
                        (\(name, val) -> 
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
        -- , Playground.View.Timeline.viewCss toValues model.timeline
        ]

toValues (State state) =
    Interpolate.Pos Interpolate.standardDefault (toFloat (state * 100))

