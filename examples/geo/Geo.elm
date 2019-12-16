module Geo exposing (..)

import Browser
import Circle2d
import CubicSpline2d
import Frame2d
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes
import LineSegment2d
import Pixels
import Point2d
import Quantity
import Svg
import Svg.Attributes
import Vector2d


type alias Model =
    { x0 : Float
    }


init : Model
init =
    { x0 = 0.5 }


type Msg
    = NewValue Float


update : Msg -> Model -> Model
update (NewValue newValue) _ =
    { x0 = newValue }


current =
    { current = { position = 246.6777623838098, velocity = 420.1534464839 }
    , end = { position = 400, velocity = 602.1920926271441 }
    , progress = 0.7
    , start = { position = 100, velocity = 0 }
    }


one =
    0.4


two =
    0.2


view : Model -> Html Msg
view { x0 } =
    let
        topLeftFrame =
            Frame2d.atPoint (Point2d.pixels 0 0)

        width =
            1600

        height =
            6000

        svg =
            Svg.g []
                [ Svg.g []
                    [ Svg.translateBy (Vector2d.pixels 100 100) <|
                        viewSpline "Material Design Standard Cubic Bezier"
                            (CubicSpline2d.fromControlPoints
                                (Point2d.pixels 0 0)
                                (Point2d.pixels (100 * one) 0)
                                (Point2d.pixels (100 * two) 100)
                                (Point2d.pixels 100 100)
                            )
                    ]
                , Svg.translateBy (Vector2d.pixels 0 300) <|
                    Svg.g []
                        [ Svg.translateBy (Vector2d.pixels 100 100) <|
                            viewSpline "Boring standard, linear"
                                (createSpline
                                    { start = Point2d.pixels 0 0
                                    , startVelocity = Vector2d.pixels 0 0
                                    , leaveLate = 0
                                    , leaveSlow = 0
                                    , end = Point2d.pixels 100 100
                                    , endVelocity = Vector2d.pixels 0 0
                                    , arriveEarly = 0
                                    , arriveSlow = 0
                                    }
                                )
                        , Svg.translateBy (Vector2d.pixels 400 100) <|
                            viewSpline "Leave slowly"
                                (createSpline
                                    { start = Point2d.pixels 0 0
                                    , startVelocity = Vector2d.pixels 0 0
                                    , leaveLate = 0
                                    , leaveSlow = 0.4
                                    , end = Point2d.pixels 100 100
                                    , endVelocity = Vector2d.pixels 0 0
                                    , arriveEarly = 0
                                    , arriveSlow = 0
                                    }
                                )
                        , Svg.translateBy (Vector2d.pixels 700 100) <|
                            viewSpline "Arrive Slowly"
                                (createSpline
                                    { start = Point2d.pixels 0 0
                                    , startVelocity = Vector2d.pixels 0 0
                                    , leaveLate = 0
                                    , leaveSlow = 0
                                    , end = Point2d.pixels 100 100
                                    , endVelocity = Vector2d.pixels 0 0
                                    , arriveEarly = 0
                                    , arriveSlow = 0.8
                                    }
                                )
                        , Svg.translateBy (Vector2d.pixels 1000 100) <|
                            viewSpline "Recreate material"
                                (createSpline
                                    { start = Point2d.pixels 0 0
                                    , startVelocity = Vector2d.pixels 0 0
                                    , leaveLate = 0
                                    , leaveSlow = 0.4
                                    , end = Point2d.pixels 100 100
                                    , endVelocity = Vector2d.pixels 0 0
                                    , arriveEarly = 0
                                    , arriveSlow = 0.8
                                    }
                                )
                        ]
                ]
    in
    Html.div []
        [ Html.div []
            [ Svg.svg
                [ Svg.Attributes.width (String.fromFloat width)
                , Svg.Attributes.height (String.fromFloat height)
                , Html.Attributes.style "display" "block"
                ]
                [ svg
                    |> Svg.relativeTo topLeftFrame
                ]
            ]
        ]


type alias Proportion =
    Float


type alias Config coordinates =
    { start : Point2d.Point2d Pixels.Pixels coordinates
    , startVelocity : Vector2d.Vector2d Pixels.Pixels coordinates
    , leaveLate : Proportion
    , leaveSlow : Proportion
    , end : Point2d.Point2d Pixels.Pixels coordinates
    , endVelocity : Vector2d.Vector2d Pixels.Pixels coordinates
    , arriveEarly : Proportion
    , arriveSlow : Proportion
    }


createSpline : Config coordinates -> CubicSpline2d.CubicSpline2d Pixels.Pixels coordinates
createSpline config =
    let
        startX =
            config.start
                |> Point2d.xCoordinate
                |> Pixels.inPixels

        endX =
            config.end
                |> Point2d.xCoordinate
                |> Pixels.inPixels

        totalX =
            endX - startX

        startVelocity =
            if config.leaveSlow == 0 then
                config.startVelocity

            else if config.startVelocity == Vector2d.zero then
                Vector2d.pixels totalX 0
                    |> Vector2d.scaleBy (config.leaveSlow * 3)

            else
                config.startVelocity
                    |> Vector2d.scaleBy (config.leaveSlow * 3)

        endVelocity =
            if config.arriveSlow == 0 then
                config.endVelocity

            else if config.endVelocity == Vector2d.zero then
                Vector2d.pixels totalX 0
                    |> Vector2d.scaleBy (config.arriveSlow * 3)

            else
                config.endVelocity
                    |> Vector2d.scaleBy (config.arriveSlow * 3)
    in
    CubicSpline2d.fromEndpoints
        config.start
        startVelocity
        config.end
        endVelocity


viewSpline label spline =
    let
        d1 =
            CubicSpline2d.startDerivative spline
                |> (\vector ->
                        let
                            point =
                                CubicSpline2d.firstControlPoint spline
                        in
                        LineSegment2d.from point (point |> Point2d.translateBy vector)
                   )

        endD1 =
            CubicSpline2d.startDerivative spline
                |> (\vector ->
                        let
                            point =
                                CubicSpline2d.firstControlPoint spline
                        in
                        point |> Point2d.translateBy vector
                   )

        d2 =
            CubicSpline2d.endDerivative spline
                |> (\vector ->
                        let
                            point =
                                CubicSpline2d.fourthControlPoint spline
                        in
                        LineSegment2d.from point (point |> Point2d.translateBy vector)
                   )

        endD2 =
            CubicSpline2d.endDerivative spline
                |> (\vector ->
                        let
                            point =
                                CubicSpline2d.fourthControlPoint spline
                        in
                        point |> Point2d.translateBy vector
                   )

        pointsOn curve =
            List.range 0 4
                |> List.map
                    (\i ->
                        let
                            perc =
                                toFloat i / 4
                        in
                        Svg.circle2d
                            [ Svg.Attributes.fill "black"
                            ]
                        <|
                            Circle2d.withRadius (Pixels.pixels 4)
                                (CubicSpline2d.pointOn spline perc)
                    )

        linearPoints =
            List.range 0 4
                |> List.map
                    (\i ->
                        let
                            perc =
                                toFloat i / 4
                        in
                        Svg.circle2d
                            [ Svg.Attributes.fill "red"
                            ]
                        <|
                            Circle2d.withRadius (Pixels.pixels 4)
                                (Point2d.xy
                                    (atPercent perc)
                                    (Pixels.pixels -10)
                                )
                    )

        atPercent p =
            Quantity.plus
                (Point2d.xCoordinate start)
                (Quantity.multiplyBy p distanceX)

        distanceX =
            Point2d.xCoordinate end |> Quantity.minus (Point2d.xCoordinate start)

        start =
            CubicSpline2d.startPoint spline

        end =
            CubicSpline2d.endPoint spline

        mappedPoints =
            List.range 0 4
                |> List.map
                    (\i ->
                        let
                            perc =
                                toFloat i / 4

                            pnt =
                                findAtT spline (atPercent perc) 1.0 0.25 0.5 0
                        in
                        Svg.circle2d
                            [ Svg.Attributes.fill "blue"
                            ]
                        <|
                            Circle2d.withRadius (Pixels.pixels 4)
                                (Point2d.pixels pnt.x pnt.y)
                    )

        lineStyle =
            [ Svg.Attributes.stroke "red"
            , Svg.Attributes.strokeWidth "3"
            , Svg.Attributes.strokeLinecap "round"
            ]
    in
    Svg.g []
        [ Svg.cubicSpline2d
            [ Svg.Attributes.fill "none"
            , Svg.Attributes.stroke "black"
            , Svg.Attributes.strokeWidth "1"
            ]
            spline
        , Svg.lineSegment2d lineStyle d1
        , Svg.lineSegment2d lineStyle d2
        , Svg.g []
            linearPoints
        , Svg.g []
            mappedPoints
        , plotControlPoints spline
        , Svg.circle2d
            [ Svg.Attributes.fill "black"
            ]
          <|
            Circle2d.withRadius (Pixels.pixels 4)
                endD1
        , Svg.circle2d
            [ Svg.Attributes.fill "black"
            ]
          <|
            Circle2d.withRadius (Pixels.pixels 4)
                endD2
        , Svg.translateBy (Vector2d.pixels 0 -20) <|
            Svg.text_ []
                [ Svg.text label ]
        ]


plotControlPoints spline =
    let
        c0 =
            CubicSpline2d.firstControlPoint spline

        c1 =
            CubicSpline2d.secondControlPoint spline

        c2 =
            CubicSpline2d.thirdControlPoint spline

        c3 =
            CubicSpline2d.fourthControlPoint spline

        circleStyle =
            [ Svg.Attributes.stroke "green"
            , Svg.Attributes.strokeWidth "2"
            , Svg.Attributes.fill "white"
            ]

        lineStyle =
            [ Svg.Attributes.stroke "#CCC"
            , Svg.Attributes.strokeDasharray "4"
            , Svg.Attributes.strokeWidth "2"
            ]
    in
    Svg.g []
        [ Svg.lineSegment2d lineStyle <|
            LineSegment2d.from
                c0
                c1
        , Svg.lineSegment2d lineStyle <|
            LineSegment2d.from
                c1
                c2
        , Svg.lineSegment2d lineStyle <|
            LineSegment2d.from
                c2
                c3
        , Svg.circle2d
            circleStyle
            (Circle2d.withRadius (Pixels.pixels 6) c0)
        , Svg.circle2d
            circleStyle
            (Circle2d.withRadius (Pixels.pixels 6) c1)
        , Svg.circle2d
            circleStyle
            (Circle2d.withRadius (Pixels.pixels 6) c2)
        , Svg.circle2d
            circleStyle
            (Circle2d.withRadius (Pixels.pixels 6) c3)
        ]


findAtT : CubicSpline2d.CubicSpline2d Pixels.Pixels Pixels.Pixels -> Quantity.Quantity Float Pixels.Pixels -> Float -> Float -> Float -> Float -> { x : Float, y : Float }
findAtT spline desiredX tolerance jumpSize t depth =
    let
        p =
            CubicSpline2d.pointOn spline t
                |> Point2d.toPixels
    in
    if depth == 10 then
        p

    else if within tolerance p.x (Pixels.inPixels desiredX) then
        p

    else if p.x > Pixels.inPixels desiredX then
        findAtT spline desiredX tolerance (jumpSize / 2) (t - jumpSize) (depth + 1)

    else
        findAtT spline desiredX tolerance (jumpSize / 2) (t + jumpSize) (depth + 1)


within : Float -> Float -> Float -> Bool
within tolerance anchor at =
    let
        low =
            anchor - tolerance

        high =
            anchor + tolerance
    in
    at >= low && at <= high



-- https://stackoverflow.com/questions/8217346/cubic-bezier-curves-get-y-for-given-x
-- P0 = (X0,Y0)
-- P1 = (X1,Y1)
-- P2 = (X2,Y2)
-- P3 = (X3,Y3)
-- X(t) = (1-t)^3 * X0 + 3*(1-t)^2 * t * X1 + 3*(1-t) * t^2 * X2 + t^3 * X3
-- Y(t) = (1-t)^3 * Y0 + 3*(1-t)^2 * t * Y1 + 3*(1-t) * t^2 * Y2 + t^3 * Y3
-- yAtT spline t =
--     let
--         p0 =
--             spline
--                 |> CubicSpline2d.firstControlPoint
--                 |> Point2d.toPixels
--         p1 =
--             spline
--                 |> CubicSpline2d.secondControlPoint
--                 |> Point2d.toPixels
--         p2 =
--             spline
--                 |> CubicSpline2d.thirdControlPoint
--                 |> Point2d.toPixels
--         p3 =
--             spline
--                 |> CubicSpline2d.fourthControlPoint
--                 |> Point2d.toPixels
--         remain =
--             1 - t
--         first =
--             remain * remain * remain * p0.y
--         second =
--             3 * remain * remain * t * p1.y
--         third =
--             remain * 3 * t * t * p2.y
--         fourth =
--             t * t * t * p3.y
--     in
--     first + second + third + fourth


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
