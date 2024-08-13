module InternalAnim.Render.Css exposing
    ( animation
    , frame
    , keyframes
    , prop
    , timingFunction
    )

import Bezier
import InternalAnim.Time as Time


timingFunction : Bezier.Spline -> String
timingFunction spline =
    "animation-timing-function:" ++ Bezier.toCss spline ++ ";"


animation : Time.Duration -> Time.Duration -> Int -> String -> String
animation duration delay count name =
    let
        n =
            if count == -1 then
                "infinite"

            else if count <= 0 then
                "1"

            else
                String.fromInt count
    in
    Time.durationToString duration
        -- we specify an easing function here because it we have to
        -- , but it is overridden by the one in keyframes
        ++ " linear "
        ++ Time.durationToString delay
        ++ " "
        ++ n
        ++ " normal forwards running "
        ++ name


keyframes : String -> String -> String
keyframes name kfs =
    "@keyframes " ++ name ++ " {\n" ++ kfs ++ "\n}"


frame : Int -> String -> String
frame percent props =
    String.fromInt percent ++ "% {\n" ++ props ++ "\n}"


prop : String -> String -> String
prop name value =
    name ++ ": " ++ value ++ ";"
