module InternalAnim.Duration exposing
    ( Duration
    , inMilliseconds
    , inSeconds
    , isZero
    , milliseconds
    , scale
    , seconds
    )

import InternalAnim.Quantity as Quantity


type alias Duration =
    Quantity.Quantity Float Seconds


type Seconds
    = Seconds


scale : Float -> Duration -> Duration
scale factor (Quantity.Quantity secs) =
    Quantity.Quantity (factor * secs)


isZero : Duration -> Bool
isZero (Quantity.Quantity secs) =
    secs == 0


inMilliseconds : Duration -> Float
inMilliseconds (Quantity.Quantity secs) =
    secs * 1000


milliseconds : Float -> Duration
milliseconds numMilliseconds =
    Quantity.Quantity (0.001 * numMilliseconds)


seconds : Float -> Duration
seconds numSeconds =
    Quantity.Quantity numSeconds


inSeconds : Duration -> Float
inSeconds (Quantity.Quantity secs) =
    secs
