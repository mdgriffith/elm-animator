module Help.Plot exposing (Model, Msg, init, spring, update, view)

{-| -}

import Browser
import Color
import Html exposing (Html, div, h1, node, p, text)
import Html.Attributes exposing (class)
import Internal.Spring
import LineChart as LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk exposing (..)
import LineChart.Legends as Legends
import LineChart.Line as Line
import Svg exposing (Attribute, Svg, g, text_, tspan)



-- main : Program () Model Msg
-- main =
--     Browser.sandbox
--         { init = init
--         , update = update
--         , view = view
--         }
-- MODEL


type alias Model =
    { hovering : Maybe { position : Float, time : Float } }


init : Model
init =
    { hovering = Nothing }



-- UPDATE


type Msg
    = Hover (Maybe { position : Float, time : Float })


update : Msg -> Model -> Model
update msg model =
    case msg of
        Hover hovering ->
            { model | hovering = hovering }



-- VIEW


view :
    Model
    -> List { position : Float, time : Float }
    -> List { position : Float, time : Float }
    -> List { position : Float, time : Float }
    -> { position : Float, time : Float }
    -> Svg Msg
view model points velocities events current =
    Html.div
        [ class "container" ]
        [ chart model points velocities events current ]


chart :
    Model
    -> List { position : Float, time : Float }
    -> List { position : Float, time : Float }
    -> List { position : Float, time : Float }
    -> { position : Float, time : Float }
    -> Html.Html Msg
chart model points velocities events current =
    LineChart.viewCustom
        { y = Axis.default 450 "Value" .position
        , x = Axis.default 1800 "Time" .time
        , container = Container.styled "line-chart-1" [ ( "font-family", "monospace" ) ]
        , interpolation = Interpolation.default
        , intersection = Intersection.default
        , legends = Legends.default
        , events =
            Events.custom
                [ Events.onMouseMove Hover Events.getNearest
                , Events.onMouseLeave (Hover Nothing)
                ]
        , junk = Junk.default
        , grid = Grid.default
        , area = Area.default
        , line = Line.hoverOne model.hovering
        , dots = Dots.hoverOne model.hovering
        }
        [ LineChart.line Color.purple Dots.none "Position" points
        , LineChart.line Color.blue Dots.none "Velocity" velocities
        , LineChart.line Color.green Dots.circle "Currently" [ current ]
        , LineChart.line Color.green Dots.plus "Events" events
        ]


spring :
    { initialPosition : Float
    , initialVelocity : Float
    , stiffness : Float
    , damping : Float
    }
    -> Html.Html msg
spring cfg =
    let
        estimatedSettling =
            Debug.log "settled"
                { time =
                    Internal.Spring.settlesAt
                        { stiffness = cfg.stiffness
                        , damping = cfg.damping
                        }
                , position = 1
                , target = 1
                , velocity = 0
                }

        springStart =
            { target = 1
            , position = cfg.initialPosition
            , velocity = cfg.initialVelocity
            , time = 0
            }

        points =
            List.foldl
                (\i ( motion, steps ) ->
                    let
                        new =
                            Internal.Spring.step 16
                                { stiffness = cfg.stiffness
                                , damping = cfg.damping
                                }
                                motion

                        newWithTime =
                            { new | time = new.time + 16 }
                    in
                    ( newWithTime, newWithTime :: steps )
                )
                ( springStart
                , [ springStart ]
                )
                (List.range 0 100)
                |> Tuple.second
    in
    LineChart.viewCustom
        { y = Axis.default 300 "Value" .position
        , x = Axis.default 500 "Time" .time
        , container = Container.styled "line-chart-1" [ ( "font-family", "monospace" ) ]
        , interpolation = Interpolation.default
        , intersection = Intersection.default
        , legends = Legends.default
        , events =
            Events.default
        , junk = Junk.default
        , grid = Grid.default
        , area = Area.default
        , line = Line.default
        , dots = Dots.default
        }
        [ LineChart.line Color.purple Dots.none "Position" points
        , LineChart.line Color.green Dots.circle "Estimated Settle" [ estimatedSettling ]
        ]
