module Geo exposing (..)

-- import Kintail.InputWidget as InputWidget

import Browser
import Circle2d
import CubicSpline2d
import Frame2d
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes
import Pixels
import Point2d
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


view : Model -> Html Msg
view { x0 } =
    let
        topLeftFrame =
            -- Frame2d.atCoordinates ( 0, 400 )
            Frame2d.atPoint (Point2d.pixels 0 0)

        -- |> Frame2d.reverseY
        width =
            1600

        height =
            1000

        spline =
            -- CubicSpline2d.fromEndpoints
            --     (Point2d.pixels 100 100)
            --     (Vector2d.pixels 100 0)
            --     (Point2d.pixels 300 300)
            --     (Vector2d.pixels 0 -300)
            CubicSpline2d.fromEndpoints
                (Point2d.pixels 100 current.start.position)
                (Vector2d.pixels 100 current.start.velocity)
                (Point2d.pixels 400 current.end.position)
                (Vector2d.pixels 400 (-1 * current.end.velocity))

        -- point on: Point2d { x = 212.5, y = 325.274011578393 }
        -- point on: Point2d { x = 175, y = 325.274011578393 }
        _ =
            Debug.log "point on" (CubicSpline2d.pointOn spline 0.5)

        _ =
            Debug.log "deriv"
                (CubicSpline2d.firstDerivative spline 0.5)

        -- point on: Point2d { x = 175, y = 325.274011578393 } index.html:523:10
        -- deriv: Vector2d { x = 200, y = 600.548023156786 }
        -- point on: Point2d { x = 212.5, y = 325.274011578393 } index.html:523:10
        -- deriv: Vector2d { x = 325, y = 600.548023156786 }
        svg =
            Svg.g []
                [ Svg.cubicSpline2d
                    [ Svg.Attributes.fill "none"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.strokeWidth "1"
                    ]
                    spline
                , Svg.circle2d
                    [ Svg.Attributes.fill "black"
                    ]
                  <|
                    Circle2d.withRadius (Pixels.pixels 4)
                        (Point2d.pixels 100 100)
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
                    -- |> Svg.scaleAbout Point2d.origin
                    |> Svg.relativeTo topLeftFrame
                ]
            ]

        -- , InputWidget.slider [ Html.Attributes.style [ ( "width", "300px" ) ] ]
        --     { min = 0.01, max = 0.99, step = 0.01 }
        --     x0
        --     |> Html.map NewValue
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
