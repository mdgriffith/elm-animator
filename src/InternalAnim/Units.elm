module InternalAnim.Units exposing
    ( Pixels
    , PixelsPerSecond
    , inPixels
    , inPixelsPerMs
    , inPixelsPerSecond
    , pixels
    , pixelsPerSecond
    , zero
    )

{-| -}

import InternalAnim.Quantity as Quantity


type PixelsPerSecondUnit
    = PixelsPerSecondUnit


type InPixels
    = InPixels


zero =
    Quantity.Quantity 0


type Unit guard
    = Unit Float


type alias Position =
    Unit Pos


type Pos
    = Pos


type alias Velocity =
    Unit Vel


type Vel
    = Vel


type alias Pixels =
    Quantity.Quantity Float InPixels


type alias PixelsPerSecond =
    Quantity.Quantity Float PixelsPerSecondUnit


inPixelsPerSecond : PixelsPerSecond -> Float
inPixelsPerSecond (Quantity.Quantity pps) =
    pps


inPixelsPerMs : PixelsPerSecond -> Float
inPixelsPerMs (Quantity.Quantity pps) =
    1000 * pps


inPixels : Pixels -> Float
inPixels (Quantity.Quantity pixs) =
    pixs


pixels : Float -> Pixels
pixels ps =
    Quantity.Quantity ps


pixelsPerSecond : Float -> PixelsPerSecond
pixelsPerSecond ps =
    Quantity.Quantity ps
