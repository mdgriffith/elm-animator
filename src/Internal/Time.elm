module Internal.Time exposing
    ( thisBeforeOrEqualThat, thisAfterOrEqualThat, equal
    , Absolute, AbsoluteTime(..), Duration, absolute, duration, progress
    , inMilliseconds
    , latest, earliest, toPosix, durationToString, reduceDurationBy
    , advanceBy, equalDuration, expand, isZeroDuration, maxDuration, millis, numberOfFrames, positiveDuration, progressWithin, rollbackBy, thisAfterThat, thisBeforeThat, zeroDuration
    )

{-|

@docs thisBeforeOrEqualThat, thisAfterOrEqualThat, equal

@docs Absolute, AbsoluteTime, Duration, absolute, duration, progress

@docs inMilliseconds

@docs latest, earliest, toPosix, durationToString, reduceDurationBy

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


reduceDurationBy : Duration -> Duration -> Duration
reduceDurationBy one two =
    Quantity.minus one two


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


earliest : Absolute -> Absolute -> Absolute
earliest ((Quantity.Quantity one) as oneQty) ((Quantity.Quantity two) as twoQty) =
    if (one - two) >= 0 then
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


equalDuration : Duration -> Duration -> Bool
equalDuration (Quantity.Quantity this) (Quantity.Quantity that) =
    (this - that) == 0


{-| The number of frames, and the offset that's needed to preserve the framerate.

Offset

-}
numberOfFrames : Float -> Absolute -> Absolute -> Absolute -> ( Float, Int )
numberOfFrames fps lastFrameTime startAt endAt =
    let
        millisecondsPerFrame =
            1000 / fps

        totalDurationInMs =
            Duration.inMilliseconds (duration startAt endAt)

        framesSinceLastFrame =
            max 0 (Duration.inMilliseconds (duration lastFrameTime startAt))
                / millisecondsPerFrame

        offset =
            1 - (framesSinceLastFrame - toFloat (floor framesSinceLastFrame))
    in
    ( offset * millisecondsPerFrame
    , max 1 (round (totalDurationInMs / millisecondsPerFrame))
    )
