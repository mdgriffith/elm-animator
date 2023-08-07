module Playground.View.Bezier exposing (view)

import Animator
import Animator.Watcher as Watcher
import Html
import InternalAnim.Bezier as Bezier
import InternalAnim.Css
import InternalAnim.Time as Time
import InternalAnim.Transition as Transition
import Pixels
import Svg exposing (Svg)
import Svg.Attributes exposing (..)


type alias RenderedGroup =
    { startedAt : Time.Absolute
    , props : List InternalAnim.Css.RenderedProp
    }


view : List RenderedGroup -> Svg msg
view group =
    Svg.svg
        [ width "1200"
        , height "1200"
        , viewBox "-200 -200 1000 1000"
        ]
        []



-- viewTransition :
--     Bezier.Point
--     ->
--         { start : Bezier.Point
--         , end : Bezier.Point
--         }
--     -> Float
--     -> Float
--     -> Transition.Transition
--     -> Svg.Svg msg
-- viewTransition offset domain introVelocity exitVelocity transition =
--     let
--         splines =
--             Transition.splines domain introVelocity exitVelocity transition
--     in
--     Svg.g []
--         (List.map (viewSpline offset) splines)


viewSpline : Bezier.Point -> Bezier.Spline -> Svg msg
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
