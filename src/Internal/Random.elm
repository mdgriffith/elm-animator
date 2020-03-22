module Internal.Random exposing (random)

{-| -}


fract x =
    x - toFloat (floor x)



-- learned this approach from https://thebookofshaders.com/10/
-- super cool!
-- and turns out 32x faster than elm/random
-- though elm/random has a much nicer distribution


random : Float -> Float -> Float -> Float
random seed low high =
    (fract (sin seed * 100000.0) + 1.5707) / pi
