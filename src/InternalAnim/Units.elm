module InternalAnim.Units exposing
    ( Pixels
    , PixelsPerSecond
    , inPixels
    , inPixelsPerSecond
    , pixels
    , pixelsPerSecond
    )

{-| -}

import InternalAnim.Quantity as Quantity


type PixelsPerSecondUnit
    = PixelsPerSecondUnit


type InPixels
    = InPixels


type alias Pixels =
    Quantity.Quantity Float InPixels


type alias PixelsPerSecond =
    Quantity.Quantity Float PixelsPerSecondUnit


inPixelsPerSecond : PixelsPerSecond -> Float
inPixelsPerSecond (Quantity.Quantity pps) =
    pps


inPixels : Pixels -> Float
inPixels (Quantity.Quantity pixs) =
    pixs


pixels : Float -> Pixels
pixels ps =
    Quantity.Quantity ps


pixelsPerSecond : Float -> PixelsPerSecond
pixelsPerSecond ps =
    Quantity.Quantity ps
