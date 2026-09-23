module InternalAnim.Render.Css exposing
    ( animation
    , frame
    , keyframes
    , prop
    , timingFunction
    )

import Bezier
import InternalAnim.Duration as Duration
import InternalAnim.Time as Time


timingFunction : Bezier.Spline -> String
timingFunction spline =
    "animation-timing-function:" ++ Bezier.toCss spline ++ ";"


animation : Time.Duration -> Time.Duration -> Int -> String -> String
animation duration delay count name =
    let
        n =
            if count < 0 then
                "infinite"

            else if count <= 0 then
                "1"

            else
                String.fromInt count
    in
    durationString duration
        -- we specify an easing function here because it we have to
        -- , but it is overridden by the one in keyframes
        ++ " linear "
        ++ durationString delay
        ++ " "
        ++ n
        ++ " normal forwards running "
        ++ name


durationString : Time.Duration -> String
durationString duration =
    String.fromFloat (Duration.inMilliseconds duration) ++ "ms"


keyframes : String -> String -> String
keyframes name kfs =
    "@keyframes " ++ name ++ " {\n" ++ kfs ++ "\n}"


frame : Float -> String -> String
frame percent props =
    String.fromFloat (clamp 0 100 percent) ++ "% {\n" ++ props ++ "\n}"


prop : String -> String -> String
prop name value =
    name ++ ": " ++ value ++ ";"
