module Help.Plot exposing
    ( Model
    , Msg
    , damping
    , easing
    , init
    , settlingTime
    , spring
    , timeline
    , update
    , view
    )

{-| -}

import Animator
import Browser
import Color
import Duration
import Html exposing (Html, div, h1, node, p, text)
import Html.Attributes exposing (class)
import Internal.Estimation as Estimate
import Internal.Interpolate as Interpolate
import Internal.Spring as Spring
import Internal.Timeline
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
import Time



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


earliest : Time.Posix -> Time.Posix -> Time.Posix
earliest one two =
    let
        oneMs =
            Time.posixToMillis one

        twoMs =
            Time.posixToMillis two
    in
    if oneMs < twoMs then
        one

    else
        two


latest : Time.Posix -> Time.Posix -> Time.Posix
latest one two =
    let
        oneMs =
            Time.posixToMillis one

        twoMs =
            Time.posixToMillis two
    in
    if oneMs > twoMs then
        one

    else
        two


getStartAndEnd events =
    List.foldl
        (\( time, ev ) maybeBounds ->
            case maybeBounds of
                Nothing ->
                    Just
                        ( time, time )

                Just ( low, high ) ->
                    Just
                        ( earliest low time
                        , latest high time
                        )
        )
        Nothing
        events
        |> Maybe.withDefault ( Time.millisToPosix 0, Time.millisToPosix 1500 )


addMs add posix =
    let
        ms =
            Time.posixToMillis posix
    in
    Time.millisToPosix (ms + add)



-- VIEW


timeline :
    { timeline : Animator.Timeline event
    , toMovement : event -> Animator.Movement
    }
    -> Svg msg
timeline config =
    let
        lines =
            Internal.Timeline.getEvents config.timeline

        ( start, end ) =
            getStartAndEnd (List.concat lines)

        rendered =
            render config.timeline
                config.toMovement
                { framesPerSecond = 60
                , start = start
                , end = addMs 1000 end
                }

        points =
            List.map
                (\e ->
                    { time = toFloat (Time.posixToMillis e.time)
                    , value = e.value.position
                    }
                )
                rendered

        estimatedVelocities =
            renderEstimatedVelocity config.timeline
                config.toMovement
                { framesPerSecond = 60
                , start = start
                , end = addMs 1000 end
                }

        velocities =
            List.map
                (\e ->
                    { time = toFloat (Time.posixToMillis e.time)
                    , value = e.value.velocity
                    }
                )
                rendered

        calcVelocities =
            calcDerivative rendered
                (.time >> Time.posixToMillis)
                (.value >> .position)

        acceleration =
            calcDerivative rendered
                (.time >> Time.posixToMillis)
                (.value >> .velocity)

        renderedEvents =
            List.indexedMap
                (\i events ->
                    LineChart.line Color.green Dots.plus "Events" (renderEvents i events)
                )
                lines
    in
    Html.div
        [ class "container" ]
        [ LineChart.viewCustom
            { y = Axis.default 450 "Value" .value
            , x = Axis.default 600 "Time" .time
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
            (renderedEvents
                ++ [ LineChart.line Color.purple Dots.none "Position" points

                   --    , LineChart.dash Color.blue Dots.none "Velocity" [ 4, 2 ] velocities
                   --    , LineChart.dash Color.red Dots.none "Est. Vel." [ 4, 4 ] estimatedVelocities
                   -- , LineChart.line Color.red Dots.none "Calc - Velocity" calcVelocities
                   -- , LineChart.line Color.orange Dots.none "Accel" acceleration
                   ]
            )
        ]


easing : (Float -> Float) -> Float -> Float -> Svg msg
easing ease start end =
    let
        rendered =
            renderEasingRange ease start end 200

        points =
            List.map
                (\e ->
                    { time = e.time
                    , value = e.position
                    }
                )
                rendered

        velocities =
            List.map
                (\e ->
                    { time = e.time
                    , value = e.velocity
                    }
                )
                rendered

        acceleration =
            List.map
                (\e ->
                    { time = e.time
                    , value = e.acceleration
                    }
                )
                rendered
    in
    Html.div
        [ class "container" ]
        [ LineChart.viewCustom
            { y = Axis.default 450 "Value" .value
            , x = Axis.default 600 "Time" .time
            , container = Container.styled "line-chart-1" [ ( "font-family", "monospace" ) ]
            , interpolation = Interpolation.default
            , intersection = Intersection.default
            , legends = Legends.default
            , events = Events.default
            , junk = Junk.default
            , grid = Grid.default
            , area = Area.default
            , line = Line.default
            , dots = Dots.default
            }
            [ LineChart.line Color.purple Dots.none "Position" points
            , LineChart.line Color.blue Dots.none "Velocity" velocities
            , LineChart.line Color.orange Dots.none "Acceleration" acceleration
            ]
        ]


renderEasingRange ease start end steps =
    let
        dt =
            (end - start) / toFloat steps

        renderPoint i points =
            let
                f =
                    toFloat i

                new =
                    { position = ease (f * dt)
                    , velocity = easingVelocity ease (f * dt) dt
                    , acceleration = easingAcceleration ease (f * dt) dt
                    , time = f * dt
                    }
            in
            new :: points
    in
    List.foldl renderPoint [] (List.range 0 steps)


easingVelocity ease i dt =
    let
        ( start, end ) =
            ( i - (dt / 2)
            , i + (dt / 2)
            )

        dx =
            ease end - ease start
    in
    dx / dt


easingAcceleration fn i dt =
    let
        ( start, end ) =
            ( i - (dt / 2)
            , i + (dt / 2)
            )

        v1 =
            easingVelocity fn start dt

        v2 =
            easingVelocity fn end dt

        ddx =
            v2 - v1
    in
    ddx / dt


avgTime t1 t2 =
    let
        t1InMs =
            Time.posixToMillis t1

        t2InMs =
            Time.posixToMillis t2

        start =
            min t1InMs t2InMs
    in
    Time.millisToPosix (start + round (abs (toFloat t1InMs - toFloat t2InMs) / 2))


calcDerivative points toMs toVal =
    case points of
        [] ->
            []

        start :: remain ->
            List.foldl
                (\point ( prev, accels ) ->
                    let
                        t1InMs =
                            toMs prev

                        t2InMs =
                            toMs point

                        dt =
                            (toFloat t2InMs - toFloat t1InMs) / 1000

                        newPoint =
                            { time =
                                -- time is given in ms
                                toFloat (t2InMs + t1InMs) / 2
                            , value =
                                -- 10
                                --     *
                                -- this value should be val/second
                                (toVal point - toVal prev)
                                    / dt
                            }
                    in
                    ( point, newPoint :: accels )
                )
                ( start, [] )
                remain
                |> Tuple.second


render :
    Animator.Timeline event
    -> (event -> Animator.Movement)
    ->
        { framesPerSecond : Float
        , start : Time.Posix
        , end : Time.Posix
        }
    ->
        List
            { time : Time.Posix
            , value : { position : Float, velocity : Float }
            }
render myTimeline toPos config =
    let
        startTimeInMs =
            Time.posixToMillis config.start

        durationInMs =
            Time.posixToMillis config.end
                - startTimeInMs

        frameCount =
            (toFloat durationInMs / 1000) * config.framesPerSecond

        frameSize =
            1000 / config.framesPerSecond

        frames =
            List.range 0 (ceiling frameCount)
    in
    List.foldl
        (\i rendered ->
            let
                currentTime =
                    Time.millisToPosix (round (toFloat startTimeInMs + (toFloat i * frameSize)))
            in
            { time = currentTime
            , value =
                Interpolate.details
                    (Internal.Timeline.atTime currentTime myTimeline)
                    (toPos >> Interpolate.withStandardDefault)
            }
                :: rendered
        )
        []
        frames


renderEvents i events =
    List.map
        (\( time, ev ) ->
            { time = toFloat (Time.posixToMillis time)
            , value = toFloat (300 + (i * 50))
            }
        )
        events


renderPoints move tl toPos =
    List.foldl
        (\i rendered ->
            let
                currentTime =
                    Time.millisToPosix (i * 16)
            in
            case move (Internal.Timeline.atTime currentTime tl) toPos of
                current ->
                    { time = toFloat i * 16
                    , position = current.position
                    }
                        :: rendered
        )
        []
        (List.range 0 400)


renderVelocities move tl toPos =
    List.foldl
        (\i rendered ->
            let
                currentTime =
                    Time.millisToPosix (i * 16)
            in
            case move (Internal.Timeline.atTime currentTime tl) toPos of
                current ->
                    { time = toFloat i * 16
                    , position = current.velocity
                    }
                        :: rendered
        )
        []
        (List.range 0 400)


renderEstimatedVelocity :
    Animator.Timeline event
    -> (event -> Animator.Movement)
    ->
        { framesPerSecond : Float
        , start : Time.Posix
        , end : Time.Posix
        }
    ->
        List
            --      in MS
            { time : Float

            -- velocity
            , value : Float
            }
renderEstimatedVelocity myTimeline toPos config =
    let
        startTimeInMs =
            Time.posixToMillis config.start

        durationInMs =
            Time.posixToMillis config.end
                - startTimeInMs

        frameCount =
            (toFloat durationInMs / 1000) * config.framesPerSecond

        frameSize =
            1000 / config.framesPerSecond

        frames =
            List.range 0 (ceiling frameCount)
    in
    List.foldl
        (\i rendered ->
            let
                currentTime =
                    Time.millisToPosix (round (toFloat startTimeInMs + (toFloat i * frameSize)))

                estimated =
                    Estimate.velocity 32 currentTime myTimeline toPos
            in
            { time = toFloat (Time.posixToMillis currentTime)
            , value = estimated
            }
                :: rendered
        )
        []
        frames


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
    , mass : Float
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
                    , mass = cfg.mass
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
                            Spring.step motion.target
                                { stiffness = cfg.stiffness
                                , damping = cfg.damping
                                , mass = cfg.mass
                                }
                                16
                                { velocity = motion.velocity
                                , position = motion.position
                                }

                        newWithTime =
                            { time = motion.time + 16
                            , target = motion.target
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



{- At what duration does 0 wobble become overdamped

-}
-- wobbleBounds wobble =
--     let
--     in
--     LineChart.viewCustom
--         { y =
--             -- Axis.default 300 "Settling" .settling
--             Axis.picky 600
--                 "Settling"
--                 .settling
--                 [ 0
--                 , 200
--                 , 400
--                 , 600
--                 , 1000
--                 , 2000
--                 , 3000
--                 , 4000
--                 ]
--         , x =
--             Axis.default 1200 "Stiffness" .stiffness
--         , container = Container.styled "line-chart-3" [ ( "font-family", "monospace" ) ]
--         , interpolation = Interpolation.default
--         , intersection = Intersection.default
--         , legends = Legends.default
--         , events =
--             Events.default
--         , junk = Junk.default
--         , grid = Grid.default
--         , area = Area.default
--         , line = Line.default
--         , dots = Dots.default
--         }
--         points


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
                                        , damping = Spring.wobble2Damping wobb kMin 1 (Duration.seconds 1)
                                        , mass = 1.5
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
                                                , damping = Spring.wobble2Damping wobb k 1 (Duration.seconds 1)
                                                , mass = 1.5
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
