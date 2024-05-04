module InternalAnim.Random exposing (random)

{-| -}


fract : Float -> Float
fract x =
    x - toFloat (floor x)



-- learned this approach from https://thebookofshaders.com/10/
-- super cool!
-- and turns out 32x faster than elm/random
-- though elm/random has a much nicer distribution


random : Float -> Float -> Float -> Float
random seed low high =
    if isInfinite seed || isNaN seed then
        0

    else
        low + (((fract (sin seed * 100000.0) + 1.5707) / pi) * (high - low))
