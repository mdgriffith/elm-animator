module Help.Plot exposing (Model, Msg, damping, init, settlingTime, spring, update, view)

{-| -}

import Browser
import Color
import Html exposing (Html, div, h1, node, p, text)
import Html.Attributes exposing (class)
import Internal.Spring as Spring
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
            { time =
                Spring.settlesAt
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
                            Spring.step 16
                                { stiffness = cfg.stiffness
                                , damping = cfg.damping
                                }
                                { target = motion.target
                                , velocity = motion.velocity
                                , position = motion.position
                                }

                        newWithTime =
                            { time = motion.time + 16
                            , target = new.target
                            , velocity = new.velocity
                            , position = new.position
                            }
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


damping :
    { kMin : Float
    , kMax : Float
    }
    -> Html.Html msg
damping { kMin, kMax } =
    let
        start =
            { critical = Spring.criticalDamping kMin 1
            , spring = kMin
            }

        stepCount =
            100

        kStep =
            (kMax - kMin) / stepCount

        points =
            List.foldl
                (\i ( kPrev, steps ) ->
                    let
                        k =
                            kPrev + kStep

                        new =
                            { critical = Spring.criticalDamping k 1
                            , spring = k
                            }
                    in
                    ( k
                    , new :: steps
                    )
                )
                ( kMin
                , [ start ]
                )
                (List.range 0 stepCount)
                |> Tuple.second
    in
    LineChart.viewCustom
        { y = Axis.default 300 "Critical Damping" .critical
        , x = Axis.default 1200 "Spring" .spring
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
        [ LineChart.line Color.purple Dots.none "Constants" points
        ]


settlingTime :
    { kMin : Float
    , kMax : Float
    , wobbles : List Float
    }
    -> Html.Html msg
settlingTime { kMin, kMax, wobbles } =
    let
        stepCount =
            100

        kStep =
            (kMax - kMin) / stepCount

        points =
            wobbles
                |> List.indexedMap
                    (\i wobb ->
                        let
                            start =
                                { stiffness = kMin
                                , settling =
                                    Spring.settlesAt
                                        { stiffness = kMin
                                        , damping = Spring.wobble2Damping wobb kMin 1
                                        }
                                }
                        in
                        List.foldl
                            (\_ ( kPrev, steps ) ->
                                let
                                    k =
                                        kPrev + kStep

                                    new =
                                        { stiffness = k
                                        , settling =
                                            Spring.settlesAt
                                                { stiffness = k
                                                , damping = Spring.wobble2Damping wobb k 1
                                                }
                                        }
                                in
                                ( k
                                , new :: steps
                                )
                            )
                            ( kMin
                            , [ start ]
                            )
                            (List.range 0 stepCount)
                            |> Tuple.second
                            |> LineChart.line
                                (if i == 0 then
                                    Color.blue

                                 else if i == 1 then
                                    Color.red

                                 else if i == 5 then
                                    Color.green

                                 else
                                    Color.yellow
                                )
                                Dots.none
                                ("Wobble " ++ String.fromFloat wobb)
                    )
    in
    LineChart.viewCustom
        { y =
            -- Axis.default 300 "Settling" .settling
            Axis.picky 600
                "Settling"
                .settling
                [ 0
                , 200
                , 400
                , 600
                , 1000
                , 2000
                , 3000
                , 4000
                ]
        , x =
            Axis.default 1200 "Stiffness" .stiffness
        , container = Container.styled "line-chart-3" [ ( "font-family", "monospace" ) ]
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
        points
