module Internal.Force exposing
    ( byDistance
    , exactly
    , force
    , forceWith
    , tick
    )

{-| -}

import Duration
import Force
import Mass
import Pixels
import Point2d
import Quantity
import Speed
import Vector2d


type alias Point coords =
    Point2d.Point2d Pixels.Pixels coords


type alias PixelForceVector coords =
    Vector2d.Vector2d PixelNewtons coords


type alias Pixels =
    Quantity.Quantity Float Pixels.Pixels


type alias PixelForce =
    Quantity.Quantity Float PixelNewtons


type alias PixelNewtons =
    Quantity.Product Mass.Kilograms Pixels.PixelsPerSecondSquared


{-| -}
tick : Duration.Duration -> List (PixelForceVector coords) -> Point coords -> Point coords
tick dur forces start =
    let
        resolvedForce =
            resolve forces
    in
    Point2d.xy
        (apply dur
            (Vector2d.xComponent resolvedForce)
            (Mass.kilograms 1)
            (Point2d.xCoordinate start)
        )
        (apply dur
            (Vector2d.yComponent resolvedForce)
            (Mass.kilograms 1)
            (Point2d.yCoordinate start)
        )


{-| force = mass \* acceleration

acceleration = dVelocity/dTime

velocity = dX/dT

(force / mass) \* dT = dV

-}
apply : Duration.Duration -> PixelForce -> Mass.Mass -> Pixels -> Pixels
apply duration pixelForce mass original =
    let
        acceleration =
            pixelForce |> Quantity.over mass

        -- we assume that we've come to a complete stop before this
        -- which means deltaVelocity == velocity
        -- though normally we'd add this delta to our previous velocity
        speed : Quantity.Quantity Float Pixels.PixelsPerSecond
        speed =
            acceleration |> Quantity.for duration
    in
    Quantity.plus original (speed |> Quantity.for duration)


type Magnitude
    = Exactly PixelForce
    | ByDistance (Pixels -> PixelForce)


byDistance =
    ByDistance


exactly =
    Exactly


forceWith :
    { towards : Point coords
    , from : Point coords
    , magnitude : Magnitude
    }
    -> PixelForceVector coords
forceWith { towards, from, magnitude } =
    let
        x =
            distanceX from towards

        y =
            distanceY from towards

        hyp =
            Point2d.distanceFrom from towards

        forceAlongHypoteneuse =
            case magnitude of
                Exactly pixelForce ->
                    pixelForce

                ByDistance fn ->
                    fn hyp

        forceX =
            Quantity.multiplyBy
                -- not entirely sure why this needs to be (* -1), but it makes the signs work out.
                ((-1 * Pixels.inPixels x) / Pixels.inPixels hyp)
                forceAlongHypoteneuse

        forceY =
            Quantity.multiplyBy
                ((-1 * Pixels.inPixels y) / Pixels.inPixels hyp)
                forceAlongHypoteneuse
    in
    if from == towards then
        Vector2d.zero

    else
        Vector2d.xy forceX forceY


force : Float -> PixelForce
force =
    Quantity.Quantity


distanceX : Point coords -> Point coords -> Pixels
distanceX one two =
    Point2d.xCoordinate one |> Quantity.minus (Point2d.xCoordinate two)


distanceY : Point coords -> Point coords -> Pixels
distanceY one two =
    Point2d.yCoordinate one |> Quantity.minus (Point2d.yCoordinate two)


{-| Resolve a list of forces on an entity into the X and Y
-}
resolve : List (PixelForceVector coords) -> PixelForceVector coords
resolve forces =
    List.foldl Vector2d.plus Vector2d.zero forces
