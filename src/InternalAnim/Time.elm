module InternalAnim.Time exposing
    ( thisBeforeOrEqualThat, thisAfterOrEqualThat, equal
    , Absolute, AbsoluteTime(..), Duration, absolute, duration, progress
    , inMilliseconds
    , latest, toPosix, durationToString
    , advanceBy, expand, isZeroDuration, maxDuration, millis, positiveDuration, progressWithin, rollbackBy, scaleDuration, thisAfterThat, thisBeforeThat, zeroDuration
    )

{-|

@docs thisBeforeOrEqualThat, thisAfterOrEqualThat, equal

@docs Absolute, AbsoluteTime, Duration, absolute, duration, progress

@docs inMilliseconds

@docs latest, toPosix, durationToString

-}

import InternalAnim.Duration as Duration
import InternalAnim.Quantity as Quantity
import Time


type AbsoluteTime
    = AbsoluteTime


type alias Absolute =
    Quantity.Quantity Float AbsoluteTime


type alias Duration =
    Duration.Duration


durationToString : Duration -> String
durationToString dur =
    dur
        |> Duration.inMilliseconds
        |> round
        |> String.fromInt
        |> (\s -> s ++ "ms")


maxDuration : Duration -> Duration -> Duration
maxDuration (Quantity.Quantity one) (Quantity.Quantity two) =
    Quantity.Quantity (max one two)


millis : Float -> Absolute
millis ms =
    Quantity.Quantity ms


positiveDuration : Duration -> Duration
positiveDuration (Quantity.Quantity d) =
    Quantity.Quantity (max 0 d)


toPosix : Absolute -> Time.Posix
toPosix (Quantity.Quantity m) =
    Time.millisToPosix (round m)


absolute : Time.Posix -> Absolute
absolute posix =
    Quantity.Quantity (toFloat (Time.posixToMillis posix))


expand : Duration -> Duration -> Duration
expand one two =
    Quantity.plus one two


advanceBy : Duration -> Absolute -> Absolute
advanceBy dur time =
    Quantity.plus time (Quantity.Quantity (Duration.inMilliseconds dur))


rollbackBy : Duration -> Absolute -> Absolute
rollbackBy dur time =
    time |> Quantity.minus (Quantity.Quantity (Duration.inMilliseconds dur))


inMilliseconds : Absolute -> Float
inMilliseconds (Quantity.Quantity ms) =
    ms


scaleDuration : Float -> Duration -> Duration
scaleDuration x ((Quantity.Quantity d) as untouched) =
    if x == 1 then
        untouched

    else
        Quantity.Quantity (x * d)


duration : Absolute -> Absolute -> Duration
duration one two =
    if one |> Quantity.greaterThan two then
        Duration.milliseconds (max 0 (inMilliseconds one - inMilliseconds two))

    else
        Duration.milliseconds (max 0 (inMilliseconds two - inMilliseconds one))


progressWithin : Duration -> Duration -> Float
progressWithin (Quantity.Quantity current) (Quantity.Quantity total) =
    if total == 0 then
        0

    else
        (current / total)
            |> max 0
            |> min 1


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
latest ((Quantity.Quantity one) as oneQty) ((Quantity.Quantity two) as twoQty) =
    if (one - two) <= 0 then
        twoQty

    else
        oneQty


thisBeforeThat : Absolute -> Absolute -> Bool
thisBeforeThat (Quantity.Quantity this) (Quantity.Quantity that) =
    (this - that) < 0


thisAfterThat : Absolute -> Absolute -> Bool
thisAfterThat (Quantity.Quantity this) (Quantity.Quantity that) =
    (this - that) > 0


thisBeforeOrEqualThat : Absolute -> Absolute -> Bool
thisBeforeOrEqualThat (Quantity.Quantity this) (Quantity.Quantity that) =
    (this - that) <= 0


thisAfterOrEqualThat : Absolute -> Absolute -> Bool
thisAfterOrEqualThat (Quantity.Quantity this) (Quantity.Quantity that) =
    (this - that) >= 0


zeroDuration : Duration
zeroDuration =
    Quantity.Quantity 0


isZeroDuration : Duration -> Bool
isZeroDuration (Quantity.Quantity dur) =
    dur == 0


equal : Absolute -> Absolute -> Bool
equal (Quantity.Quantity this) (Quantity.Quantity that) =
    (this - that) == 0
