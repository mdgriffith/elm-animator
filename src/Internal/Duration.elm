module Internal.Duration exposing (Duration, inMilliseconds, inSeconds, milliseconds)

import Internal.Quantity as Quantity


type alias Duration =
    Quantity.Quantity Float Seconds


type Seconds
    = Seconds


inMilliseconds : Duration -> Float
inMilliseconds (Quantity.Quantity seconds) =
    seconds * 1000


milliseconds : Float -> Duration
milliseconds numMilliseconds =
    Quantity.Quantity (0.001 * numMilliseconds)


inSeconds : Duration -> Float
inSeconds (Quantity.Quantity seconds) =
    seconds
