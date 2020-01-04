module Internal.Time exposing
    ( thisBeforeThat, thisAfterThat, equal
    , Absolute, AbsoluteTime(..), Duration, absolute, duration, progress
    , inMilliseconds
    , latest, earliest, toPosix
    , advanceBy, rollbackBy
    )

{-|

@docs thisBeforeThat, thisAfterThat, equal

@docs Absolute, AbsoluteTime, Duration, absolute, duration, progress

@docs inMilliseconds

@docs latest, earliest, toPosix

-}

import Duration
import Quantity
import Time


type AbsoluteTime
    = AbsoluteTime


type alias Absolute =
    Quantity.Quantity Float AbsoluteTime


type alias Duration =
    Duration.Duration


toPosix : Absolute -> Time.Posix
toPosix (Quantity.Quantity m) =
    Time.millisToPosix (round m)


absolute : Time.Posix -> Absolute
absolute posix =
    Quantity.Quantity (toFloat (Time.posixToMillis posix))


advanceBy : Duration -> Absolute -> Absolute
advanceBy dur time =
    Quantity.plus time (Quantity.Quantity (Duration.inMilliseconds dur))


rollbackBy : Duration -> Absolute -> Absolute
rollbackBy dur time =
    time |> Quantity.minus (Quantity.Quantity (Duration.inMilliseconds dur))


inMilliseconds : Absolute -> Float
inMilliseconds (Quantity.Quantity ms) =
    ms


duration : Absolute -> Absolute -> Duration
duration one two =
    Duration.milliseconds (max 0 (inMilliseconds two - inMilliseconds one))


progress : Absolute -> Absolute -> Absolute -> Float
progress (Quantity.Quantity start) (Quantity.Quantity end) (Quantity.Quantity current) =
    let
        total =
            abs (end - start)
    in
    if total == 0 then
        0

    else
        ((current - start) / total)
            |> max 0
            |> min 1


latest : Absolute -> Absolute -> Absolute
latest one two =
    if thisBeforeThat one two then
        two

    else
        one


earliest : Absolute -> Absolute -> Absolute
earliest one two =
    if thisBeforeThat one two then
        one

    else
        two


thisBeforeThat : Absolute -> Absolute -> Bool
thisBeforeThat this that =
    this |> Quantity.lessThanOrEqualTo that


thisAfterThat : Absolute -> Absolute -> Bool
thisAfterThat this that =
    this |> Quantity.greaterThanOrEqualTo that


equal : Absolute -> Absolute -> Bool
equal =
    (==)
