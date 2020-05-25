module Internal.Time exposing
    ( thisBeforeOrEqualThat, thisAfterOrEqualThat, equal
    , Absolute, AbsoluteTime(..), Duration, absolute, duration, progress
    , inMilliseconds
    , latest, earliest, toPosix
    , advanceBy, millis, numberOfFrames, rollbackBy, thisAfterThat, thisBeforeThat, zeroDuration
    )

{-|

@docs thisBeforeOrEqualThat, thisAfterOrEqualThat, equal

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


millis : Float -> Absolute
millis ms =
    Quantity.Quantity ms


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
    if one |> Quantity.greaterThan two then
        Duration.milliseconds (max 0 (inMilliseconds one - inMilliseconds two))

    else
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


zeroDuration : Duration -> Bool
zeroDuration (Quantity.Quantity dur) =
    dur == 0


equal : Absolute -> Absolute -> Bool
equal (Quantity.Quantity this) (Quantity.Quantity that) =
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
