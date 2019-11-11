module Internal.Time exposing
    ( thisBeforeThat, thisAfterThat, equal
    , Absolute, Duration, absolute, duration, progress
    , inMilliseconds
    , latest, earliest
    , AbsoluteTime(..), advanceBy, durationInMs, durationToMs
    )

{-|

@docs thisBeforeThat, thisAfterThat, equal

@docs Absolute, AbsoluteTime(..), Duration, absolute, duration, progress

@docs inMilliseconds

@docs latest, earliest

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


absolute : Time.Posix -> Absolute
absolute posix =
    Quantity.Quantity (toFloat (Time.posixToMillis posix))


advanceBy : Duration -> Absolute -> Absolute
advanceBy (Quantity.Quantity dur) (Quantity.Quantity time) =
    Quantity.Quantity (time + dur)


inMilliseconds : Absolute -> Float
inMilliseconds (Quantity.Quantity ms) =
    ms


durationInMs : Float -> Duration
durationInMs =
    Quantity.Quantity


durationToMs : Duration -> Float
durationToMs (Quantity.Quantity ms) =
    ms


duration : Absolute -> Absolute -> Duration
duration (Quantity.Quantity one) (Quantity.Quantity two) =
    Quantity.Quantity (abs (two - one))


progress : Absolute -> Absolute -> Absolute -> Float
progress (Quantity.Quantity start) (Quantity.Quantity end) (Quantity.Quantity current) =
    let
        total =
            abs (end - start)
    in
    if total == 0 then
        0

    else
        (current - start) / total


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
    this |> Quantity.lessThan that


thisAfterThat : Absolute -> Absolute -> Bool
thisAfterThat this that =
    this |> Quantity.greaterThan that


equal : Absolute -> Absolute -> Bool
equal =
    (==)
