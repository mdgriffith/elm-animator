module Playground.Bezier exposing (main)

import Animator
import Html
import Internal.Bezier as Bezier
import Internal.Css
import Internal.Interpolate as Interpolate
import Internal.Transition as Transition
import Pixels
import Svg
import Svg.Attributes exposing (..)


{-| -}
main =
    Svg.svg
        [ width "1200"
        , height "1200"
        , viewBox "-200 -200 1000 1000"
        ]
        [ --     viewSpline { x = 0, y = 0 } <|
          --     Interpolate.createSpline
          --         { start =
          --             { x = 10
          --             , y = 10
          --             }
          --         , startVelocity =
          --             { x = 1
          --             , y = 1
          --             }
          --         , departure =
          --             Interpolate.standardDefault
          --         , end =
          --             { x = 90
          --             , y = 90
          --             }
          --         , endVelocity =
          --             { x = 1
          --             , y = 0
          --             }
          --         , arrival =
          --             Interpolate.standardDefault
          --         }
          -- , viewSpline { x = 120, y = 0 } <|
          --     Interpolate.createSpline
          --         { start =
          --             { x = 10
          --             , y = 10
          --             }
          --         , startVelocity =
          --             { x = 1
          --             , y = 0
          --             }
          --         , departure =
          --             Interpolate.standardDefault
          --         , end =
          --             { x = 90
          --             , y = 90
          --             }
          --         , endVelocity =
          --             { x = 1
          --             , y = 0
          --             }
          --         , arrival =
          --             Interpolate.standardDefault
          --         }
          -- ,
          viewTransition
            { x = 0
            , y = 120
            }
            { start =
                { x = 10
                , y = 10
                }
            , end =
                { x = 90
                , y = 90
                }
            }
            1
            1
            Transition.standard
        , viewTransition
            { x = 120
            , y = 120
            }
            { start =
                { x = 10
                , y = 10
                }
            , end =
                { x = 90
                , y = 90
                }
            }
            0
            0
            Transition.standard
        ]


viewTransition :
    Bezier.Point
    ->
        { start : Bezier.Point
        , end : Bezier.Point
        }
    -> Float
    -> Float
    -> Transition.Transition
    -> Svg.Svg msg
viewTransition offset domain introVelocity exitVelocity transition =
    let
        splines =
            Transition.splines domain introVelocity exitVelocity transition
    in
    Svg.g []
        (List.map (viewSpline offset) splines)


viewSpline : Bezier.Point -> Bezier.Spline -> Svg.Svg msg
viewSpline offset ((Bezier.Spline one two three four) as spline) =
    Svg.g []
        [ Svg.path
            [ d (Bezier.toPath spline)
            , fill "none"
            , stroke "black"
            , strokeWidth "3"
            , strokeDasharray "none"
            , transform
                ("translate("
                    ++ String.fromFloat offset.x
                    ++ " "
                    ++ String.fromFloat offset.y
                    ++ ")"
                )
            ]
            []
        , Svg.circle
            [ cx (String.fromFloat (one.x + offset.x))
            , cy (String.fromFloat (one.y + offset.y))
            , r "5"
            , fill "red"
            , stroke "none"
            ]
            []
        , Svg.circle
            [ cx (String.fromFloat (four.x + offset.x))
            , cy (String.fromFloat (four.y + offset.y))
            , r "5"
            , fill "red"
            , stroke "none"
            ]
            []
        , Svg.circle
            [ cx (String.fromFloat (two.x + offset.x))
            , cy (String.fromFloat (two.y + offset.y))
            , r "5"
            , fill "none"
            , stroke "red"
            , strokeWidth "2"
            , strokeDasharray "3,3"
            ]
            []
        , Svg.circle
            [ cx (String.fromFloat (three.x + offset.x))
            , cy (String.fromFloat (three.y + offset.y))
            , r "5"
            , fill "none"
            , stroke "red"
            , strokeWidth "2"
            , strokeDasharray "3,3"
            ]
            []
        ]
