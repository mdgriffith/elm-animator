module Internal.Units exposing
    ( Pixels
    , PixelsPerSecond
    , inPixels
    , inPixelsPerMs
    , zero
    )

{-| -}

import Pixels
import Quantity


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
    Quantity.Quantity Float Pixels.Pixels


type alias PixelsPerSecond =
    Quantity.Quantity Float Pixels.PixelsPerSecond


inPixelsPerSecond : PixelsPerSecond -> Float
inPixelsPerSecond pps =
    Pixels.inPixelsPerSecond pps


inPixelsPerMs : PixelsPerSecond -> Float
inPixelsPerMs pps =
    1000 * Pixels.inPixelsPerSecond pps


inPixels : Pixels -> Float
inPixels =
    Pixels.inPixels
