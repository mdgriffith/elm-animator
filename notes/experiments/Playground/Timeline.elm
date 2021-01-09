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

main =
    Browser.document
        { init =
            \() ->
                ( { timeline = 
                    Animator.init (State 0)
                        |> Animator.queue
                            [ Animator.event (Animator.millis 100) (State 1)
                            , Animator.event (Animator.millis 100) (State 2)
                            , Animator.event (Animator.millis 100) (State 3)
                            ]
                        |> Timeline.update (Time.millisToPosix 0)
                        |> Animator.interrupt 
                            [ Animator.event (Animator.millis 100) (State 4)
                            , Animator.event (Animator.millis 100) (State 5)
                            , Animator.event (Animator.millis 100) (State 6)
                            ]
                        |> Timeline.update (Time.millisToPosix 150)
                        
                  , lastUpdated = Time.millisToPosix 0
                  , tooltip = Nothing
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions = 
            \_ -> Sub.none
        }

type alias Model =
    { timeline : Animator.Timeline State
    , lastUpdated : Time.Posix
    , tooltip : Maybe Tooltip
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


onGrid column row =
    { x = (100 * column) + 50
    , y = (100 * row) + 50
    }

viewBody model =
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
        , Svg.svg
            [ SvgA.width "97%"
            , SvgA.height "1000px"
            , SvgA.viewBox "0 0 1000 1000"
            -- , SvgA.style "border: 4px dashed #eee;"
            ]
            [ viewValues model.timeline
            , viewTimeline model.timeline
            , viewSplines model.timeline
            
            ]
        -- , viewCss model.timeline
        ]

type alias LayoutCache =
    { x : List (Int, Float)
    , y : List (Int, Float)
    }



viewValues timeline =
    let
        frames =
           capture Highlight (Debug.log "START OLD" 0) 500
                (\(State state) ->
                    Interpolate.Pos Interpolate.standardDefault (toFloat (state * 100))
                )
                Interpolate.moving
                timeline
                []

        newFrames =
           captureNew Faded (Debug.log "START NEW" 0) 500
                (\(State state) ->
                    Interpolate.Pos Interpolate.standardDefault (toFloat (state * 100))
                )
                Interpolate.moving
                timeline
                []
            
            
    in
    Svg.g 
        [ SvgA.id "values"
        , SvgA.style "transform: translate(500px, 30px);"
        ]
        ( newFrames ++ frames
        )

capture style start finish toMotion interp timeline rendered =
    if start >= finish then
        rendered
    else
        let
            at = 
                timeline
                    |> Timeline.atTime (Time.millisToPosix start)
                    |> Timeline.foldpOld toMotion interp
        in
        capture style
            (start + 10) 
            finish 
            toMotion
            interp
            timeline
            (dot style 
                { x = toFloat start
                , y = Pixels.inPixels at.position
                }
                :: rendered
            )

captureNew style start finish toMotion interp timeline rendered =
    if start >= finish then
        rendered
    else
        let
            at = 
                timeline
                    |> Timeline.atTime (Time.millisToPosix start)
                    |> Timeline.foldp toMotion interp
        in
        captureNew style
            (start + 10) 
            finish 
            toMotion
            interp
            timeline
            (dot style 
                { x = toFloat start
                , y = Pixels.inPixels at.position
                }
                :: rendered
            )




viewSplines timeline =
    let
        splines =
            Css.curves
                (\(State state) ->
                    Interpolate.Pos Interpolate.standardDefault (toFloat (state * 100))
                )
                timeline
                |> List.filter (List.isEmpty >> not)
    in
    Svg.g [ SvgA.id "values"
          , SvgA.style "transform: translate(500px, 30px);"
          ]
            (renderSplines 
                { x = []
                , y = []
                }
                splines
            )

renderSplines cache groups =
    case groups of
        [] ->
            []
        
        splines :: remain ->
            renderSplines cache remain ++
                renderSplineGroup splines 
         

renderSplineGroup group =
    case group of
        [] ->
            []

        mySpline :: rest ->
            (spline Normal mySpline :: renderSplineGroup rest)


endPoint (Bezier.Spline _ _ _ end) =
    end


viewCss timeline =
    let
        css =
            Css.css "prop" 
                (\x -> String.fromFloat x ++ "px")
                (\(State state) ->
                    Interpolate.Pos Interpolate.standardDefault (toFloat (state * 100))
                
                )
                timeline
    in
    div 
        [ Attr.style "position" "fixed"
        , Attr.style "left" "100px"
        , Attr.style "bottom" "100px"
        , Attr.style "white-space" "pre-line" 
        , Attr.style "font-family" "monospace" 
        , Attr.style "font-size" "10px"
        ] 
        [ Html.h2 [] [Html.text "HASH"]
        , div [] 
            [ Html.text  css.hash
            ]
        , Html.h2 [] [Html.text "ANIM"]
        , div [] 
            [ Html.text css.animation
            ]
        , Html.h2 [] [Html.text "KEYFRAMES"]
        , div [] 
            [ Html.text css.keyframes
            ]
        ]

viewTimeline (Timeline.Timeline timeline) =
    Svg.g [] 
        (case timeline.events of
            Timeline.Timetable lines ->
                case lines of
                    [] ->
                        [ Svg.text "whoops, nothing here" ]
                    (top :: remaining) ->
                        let
                            rendered =
                                viewLines timeline.now top remaining
                                    { timeMap = Timemap []
                                    , row = 0
                                    , rendered = []
                                    }

                            (new, point) =
                                position timeline.now {rendered | row = 0}

                            cursor =
                                dot Highlight point
                        in
                        List.reverse (cursor :: rendered.rendered)
        )


type Timemap =
    Timemap (List (Time.Absolute, Float))


{-|

-}
lookup : Time.Absolute -> Timemap -> (Timemap, Float)
lookup time (Timemap timemap) =
    case timemap of
        [] ->
            (Timemap [(time, 0)], 0)
        (lastTime, lastVal) :: remain ->
            if Time.thisAfterThat time lastTime then
                ( Timemap ((time, lastVal + 1):: timemap)
                , lastVal + 1
                )
            else if lastTime == time then
                ( Timemap timemap
                , lastVal
                )

            else
                ( Timemap timemap
                , lookupHelper time timemap
                )


lookupHelper time timemap =
    case timemap of
        [] ->
            0
        (lastTime, lastVal) :: remain ->
            if Time.equal lastTime time then
                lastVal
            else 
                case remain of
                    [] ->
                        0
                    (prevTime, prevValue) :: _ ->
                        
                        if (Time.thisBeforeThat time lastTime  && Time.thisAfterThat time prevTime) then
                            prevValue + 
                                progress
                                    (Time.inMilliseconds prevTime)
                                    (Time.inMilliseconds lastTime)
                                    (Time.inMilliseconds time)

                        else 
                            lookupHelper time remain



progress low high middle =
    (middle - low) 
        / (high - low )



viewLines now (Timeline.Line startsAt first rest) lines cursor =
    let
        newCursor = 
            renderLine now startsAt first rest cursor

    in
    case lines of
        [] ->
            newCursor

        (next :: upcoming) ->
            { newCursor | row = newCursor.row + 1 }
                |> viewRowTransition next newCursor.row
                |> viewLines now next upcoming 
            
    
viewRowTransition (Timeline.Line startsAt first rest) startingRow cursor =
    let
        (newTimemap, start) =
            position 
                startsAt
                { cursor | row = startingRow }

        (finalTimemap, end) =
            position 
                (Timeline.startTime first)
                { cursor | timeMap = newTimemap }


        midOne =
            { x = start.x + ((end.x - start.x) / 2)
            , y = start.y
            }

        midTwo =
            { x = end.x - ((end.x - start.x) / 2)
            , y = end.y
            }

    in
    { row = cursor.row
    , timeMap = finalTimemap
    , rendered = 
        dot Normal start
            :: curve Faded 
                start
                midOne
                midTwo
                end
            
            :: cursor.rendered
    }

renderLine now startsAt first rest cursor =
    let
        transitions =
            List.map2 Tuple.pair
                (first :: rest)
                rest

        newCursor =
            List.foldl (renderTransition now) cursor transitions
            
    in
    List.foldl (renderEvent now) newCursor rest
        |> renderEvent now first
          
        

position time cursor =
    let
        coords = onGrid col cursor.row

        (newTime, col) = lookup time cursor.timeMap

    in
    ( newTime
    , coords
    )


renderTransition now (first, second) cursor =
    let 
        (newTimemap, start) =
            position 
                (Timeline.endTime first)
                cursor

        (finalTimemap, end) =
            position 
                (Timeline.startTime second)
                { cursor | timeMap = newTimemap }
    in
    { row = cursor.row
    , timeMap = finalTimemap
    , rendered = 
        line Faded start end
            :: cursor.rendered
    }


renderEvent now event cursor =
    let 
        (newTimemap, start) =
            position 
                (Timeline.startTime event)
                cursor

        (finalTimemap, end) =
            position 
                (Timeline.endTime event)
                { cursor | timeMap = newTimemap }



    in
    if (Timeline.startTime event) == (Timeline.endTime event) then
        { row = cursor.row
        , timeMap = finalTimemap
        , rendered = 
           dot Faded end
                :: cursor.rendered
        }

    else
        { row = cursor.row
        , timeMap = finalTimemap
        , rendered = 
            dot Faded start
                :: dot Faded end
                :: line Faded start end
                :: cursor.rendered
        }



dot : Style -> Interpolate.Point -> Svg.Svg Msg
dot style point =
    Svg.circle
        [ SvgA.cx (String.fromFloat point.x)
        , SvgA.cy (String.fromFloat point.y)
        , SvgA.r 
            (if style == Mini then
                "3"
            else
                "8"
            )
        , SvgA.fill 
            (case style of
                Normal ->
                    "black"
                Highlight ->
                    "red"
                Faded ->
                    "white"
                Mini ->
                    "black"
            )
     , SvgA.stroke 
        (case style of
            Normal ->
                 "black"
            Highlight ->
                "black"
            Faded ->
                "black"
            Mini ->
                "black"

        )
    , SvgA.strokeDasharray 
        (case style of
            Normal ->
                "none"
            
            Highlight ->
                "none"
            
            Faded ->
                "none"
            Mini ->
                "none"

        )
    , SvgA.strokeWidth "3"
    ]
    []

line : Style -> Interpolate.Point -> Interpolate.Point -> Svg.Svg Msg
line style one two =
    Svg.line
        [ SvgA.x1 (String.fromFloat one.x)
        , SvgA.y1 (String.fromFloat one.y)
        , SvgA.x2 (String.fromFloat two.x)
        , SvgA.y2 (String.fromFloat two.y)
        , SvgA.stroke 
            (case style of
                Normal ->
                    "black"
                Highlight ->
                    "red"
                Faded ->
                    "black"
                Mini ->
                    "black"

            )
        , SvgA.strokeDasharray 
            (case style of
                Normal ->
                    "none"
                
                Highlight ->
                    "none"
                
                Faded ->
                    "5,5"

                Mini ->
                    "none"
            )
        , SvgA.strokeWidth "3"
        ]
        []




spline : Style -> Bezier.Spline -> Svg.Svg Msg
spline style (Bezier.Spline c0 c1 c2 c3) =
    Svg.g [] 
        [ curve style c0 c1 c2 c3
        , line Faded c0 c1
        , line Faded c1 c2
        , line Faded c2 c3
        , dot Normal c0
        , dot Faded c1
        , dot Faded c2
        , dot Normal c3
        ]

curve : Style -> Interpolate.Point -> Interpolate.Point -> Interpolate.Point -> Interpolate.Point -> Svg.Svg Msg
curve style c0 c1 c2 c3 =
    Svg.path
        [ SvgA.d 
            (String.join " "
                [ "M "
                ++ renderPoint c0
                ++ " C "
                    ++ renderPoint c1
                    ++ ", "
                    ++ renderPoint c2
                    ++ ", "
                    ++ renderPoint c3
                ]

            )
        , SvgA.strokeWidth "3"
        , SvgA.stroke 
            (case style of
                Normal ->
                    "black"
                Highlight ->
                    "red"
                Faded ->
                    "black"
                Mini ->
                    "black"

            )
        , SvgA.strokeDasharray 
            (case style of
                Normal ->
                    "none"
                
                Highlight ->
                    "none"
                
                Faded ->
                    "5,5"

                Mini ->
                    "none"
            )
        , SvgA.fill "rgba(0,0,0,0)"
        ]
        []

renderBezierString segments str =
    case segments of
        [] ->
            str

        segment :: remaining ->
            renderBezierString remaining
                (str
                    ++ " C "
                    ++ renderPoint segment.oneControl
                    ++ " "
                    ++ renderPoint segment.twoControl
                    ++ " "
                    ++ renderPoint segment.two
                )


renderPoint : Interpolate.Point -> String
renderPoint p =
    String.fromFloat p.x ++ " " ++ String.fromFloat p.y


