module InternalAnim.Render.Css exposing
    ( animation
    , easing
    , frame
    , keyframes
    , prop
    , timingFunction
    )

import Bezier
import InternalAnim.Duration as Duration
import InternalAnim.Time as Time
import InternalAnim.Transition as Transition
import InternalAnim.Units as Units


{-| Native easing is independent of the element's current position. CSS applies
this normalized curve to its actual start and destination, including retargets.
-}
easing : Time.Duration -> Transition.Transition -> String
easing duration curve =
    case curve of
        Transition.Transition spline ->
            Bezier.toCss spline

        Transition.Wobble _ ->
            let
                milliseconds =
                    Duration.inMilliseconds duration

                count =
                    clamp 2 240 (ceiling (milliseconds / (1000 / 60)))

                sample index =
                    if index == 0 then
                        "0"

                    else if index == count then
                        "1"

                    else
                        Transition.atX (toFloat index / toFloat count)
                            (Time.millis 0)
                            (Time.millis milliseconds)
                            curve
                            { position = Units.pixels 0, velocity = Units.pixelsPerSecond 0 }
                            1
                            |> .position
                            |> Units.inPixels
                            |> (\value -> toFloat (round (value * 1000000)) / 1000000)
                            |> String.fromFloat
            in
            if milliseconds <= 0 then
                "linear"

            else
                "linear(" ++ String.join "," (List.map sample (List.range 0 count)) ++ ")"


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
