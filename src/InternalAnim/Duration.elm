module InternalAnim.Duration exposing (Duration, inMilliseconds, inSeconds, isZero, milliseconds, scale)

import InternalAnim.Quantity as Quantity


type alias Duration =
    Quantity.Quantity Float Seconds


type Seconds
    = Seconds


scale : Float -> Duration -> Duration
scale factor (Quantity.Quantity seconds) =
    Quantity.Quantity (factor * seconds)


isZero : Duration -> Bool
isZero (Quantity.Quantity seconds) =
    seconds == 0


inMilliseconds : Duration -> Float
inMilliseconds (Quantity.Quantity seconds) =
    seconds * 1000


milliseconds : Float -> Duration
milliseconds numMilliseconds =
    Quantity.Quantity (0.001 * numMilliseconds)


inSeconds : Duration -> Float
inSeconds (Quantity.Quantity seconds) =
    seconds
