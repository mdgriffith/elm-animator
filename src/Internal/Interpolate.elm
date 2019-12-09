module Internal.Interpolate exposing
    ( color
    , Movement(..), move, xy, xyz
    , derivativeOfEasing
    , defaultArrival, defaultDeparture
    )

{-|

@docs color

@docs Movement, move, xy, xyz

@docs derivativeOfEasing

-}

import Color
import CubicSpline2d
import Duration
import Internal.Spring as Spring
import Internal.Time as Time
import Internal.Timeline as Timeline
import Pixels
import Point2d
import Quantity
import Vector2d


unwrapQuantity (Quantity.Quantity value) =
    value


{-|

    oscillate around a point

    or go to a specific position

-}
type Movement
    = Oscillate Time.Duration (Float -> Float)
    | Position Departure Arrival Float


defaultDeparture : Departure
defaultDeparture =
    { late = 0
    , speed = 0
    }


defaultArrival : Arrival
defaultArrival =
    { wobbliness = 0
    , early = 0
    , speed = 0
    }


{-| Number betwen 0 and 1
-}
type alias Proportion =
    Float


type alias Arrival =
    { wobbliness : Proportion
    , early : Proportion
    , speed : Proportion
    }


type alias Departure =
    { late : Proportion
    , speed : Proportion
    }



{- Possible characteristics


   We can't leave early or arrive late because that violates our bounds.  Doesn't seem to be an issue.




   Arrival ->
       Wobbly Float
       Early
       ~Late~
       ?Quickly


   Departure ->
       Quickly
       Late
       ~Early~

    - Arrive Wobbly, Float
    - Leave Late (Linger before departing)
    - Depart quickly
    - Arrive Early
    -




-}


wrapUnitAfter dur total =
    let
        duration =
            round (Duration.inMilliseconds dur)

        totalDuration =
            round (Duration.inMilliseconds total)
    in
    if duration == 0 || totalDuration == 0 then
        0

    else
        toFloat (totalDuration |> modBy duration)
            / toFloat duration


type alias Progress =
    Float


type alias State =
    { position : Pixels
    , velocity : PixelsPerSecond
    }


type alias XY thing =
    { x : thing
    , y : thing
    }


xy : (event -> XY Movement) -> Timeline.Occurring event -> Maybe (Timeline.Occurring event) -> Timeline.Phase (XY State) -> XY State
xy lookup current maybeLookAhead phase =
    { x = move (lookup >> .x) current maybeLookAhead (Timeline.mapPhase .x phase)
    , y = move (lookup >> .y) current maybeLookAhead (Timeline.mapPhase .y phase)
    }


type alias XYZ thing =
    { x : thing
    , y : thing
    , z : thing
    }


xyz : (event -> XYZ Movement) -> Timeline.Occurring event -> Maybe (Timeline.Occurring event) -> Timeline.Phase (XYZ State) -> XYZ State
xyz lookup current maybeLookAhead phase =
    { x = move (lookup >> .x) current maybeLookAhead (Timeline.mapPhase .x phase)
    , y = move (lookup >> .y) current maybeLookAhead (Timeline.mapPhase .y phase)
    , z = move (lookup >> .z) current maybeLookAhead (Timeline.mapPhase .z phase)
    }


{-|

    `phase` captures if we are looking for the state:
        - transitioning to the target event
        - or while the target event has been active

-}
move : (event -> Movement) -> Timeline.Occurring event -> Maybe (Timeline.Occurring event) -> Timeline.Phase State -> State
move lookup (Timeline.Occurring target targetTime maybeDwell) maybeLookAhead phase =
    let
        targetPosition =
            case lookup target of
                Oscillate _ toX ->
                    Pixels.pixels (toX 0)

                Position _ _ x ->
                    Pixels.pixels x
    in
    case phase of
        Timeline.Start ->
            { position = targetPosition
            , velocity = Pixels.pixelsPerSecond 0
            }

        Timeline.After state ->
            -- position,velocity after this event completely,
            -- including full dwell time if there is any.
            { position =
                case lookup target of
                    Oscillate period toX ->
                        case maybeDwell of
                            Nothing ->
                                -- we havent had time to oscillate if there is no dwell time.
                                Pixels.pixels (toX 0)

                            Just dwell ->
                                Pixels.pixels (toX (wrapUnitAfter period dwell))

                    Position _ _ x ->
                        Pixels.pixels x
            , velocity =
                case maybeDwell of
                    Nothing ->
                        case maybeLookAhead of
                            Nothing ->
                                Pixels.pixelsPerSecond 0

                            Just (Timeline.Occurring lookAhead aheadTime maybeLookAheadDwell) ->
                                case lookup lookAhead of
                                    Oscillate freq toX ->
                                        -- calc forward velocity?
                                        velocityBetween targetPosition targetTime (Pixels.pixels (toX 0)) aheadTime

                                    Position _ _ aheadPosition ->
                                        -- we're not dwelling here, and we're moving on to `ahead
                                        velocityBetween targetPosition targetTime (Pixels.pixels aheadPosition) aheadTime

                    Just dwell ->
                        case lookup target of
                            Oscillate period toX ->
                                derivativeOfEasing toX period (wrapUnitAfter period dwell)

                            Position _ _ aheadPosition ->
                                Pixels.pixelsPerSecond 0
            }

        Timeline.TransitioningTo progress state ->
            let
                -- at end of transition
                -- we don't care about dwell time
                -- because we're not there yet.
                velocityAtEndOfTransition =
                    case maybeLookAhead of
                        Nothing ->
                            case lookup target of
                                Position _ arriving _ ->
                                    Pixels.pixelsPerSecond 0

                                Oscillate _ _ ->
                                    Pixels.pixelsPerSecond 0

                        Just (Timeline.Occurring lookAhead aheadTime maybeLookAheadDwell) ->
                            case lookup lookAhead of
                                Oscillate period toX ->
                                    derivativeOfEasing toX period 0

                                Position _ arriving aheadPosition ->
                                    velocityBetween targetPosition targetTime (Pixels.pixels aheadPosition) aheadTime

                targetTimeInMs =
                    Time.inMilliseconds targetTime

                startTimeInMs =
                    Time.inMilliseconds progress.previousTime

                curve =
                    CubicSpline2d.fromEndpoints
                        (Point2d.unitless startTimeInMs (Pixels.inPixels state.position))
                        (Vector2d.unitless startTimeInMs (Pixels.inPixelsPerSecond state.velocity))
                        (Point2d.unitless targetTimeInMs (Pixels.inPixels targetPosition))
                        (Vector2d.unitless targetTimeInMs (Pixels.inPixelsPerSecond velocityAtEndOfTransition))

                current =
                    { position =
                        CubicSpline2d.pointOn curve progress.percent
                            |> Point2d.yCoordinate
                            |> Quantity.toFloat
                            |> Pixels.pixels
                    , velocity =
                        CubicSpline2d.firstDerivative curve progress.percent
                            |> Vector2d.yComponent
                            |> Quantity.toFloat
                            |> Pixels.pixelsPerSecond
                    }
            in
            current

        Timeline.Resting restingDuration state ->
            case lookup target of
                Position _ _ pos ->
                    { position = Pixels.pixels pos
                    , velocity = Pixels.pixelsPerSecond 0
                    }

                Oscillate period toX ->
                    { position = Pixels.pixels (toX (wrapUnitAfter period restingDuration))
                    , velocity = derivativeOfEasing toX period (wrapUnitAfter period restingDuration)
                    }


{-|

    `ease` -> easing function of {0:1} -> value
    `period` -> how long is one cycle?  We're only looking at one cycle, so this could also be named "totalDuration"
    `target` -> Where do we want to calculate a derivative?

In order to calcualte the derivative, we take 3 evenly spaced samples, spaced 16ms apart.
16ms seems like a good choice because if the period is faster than that, the position will simply skip around randomly and not be smooth.

-}
derivativeOfEasing : (Float -> Float) -> Time.Duration -> Float -> PixelsPerSecond
derivativeOfEasing ease period target =
    if Duration.inMilliseconds period < 48 then
        Pixels.pixelsPerSecond 0

    else
        let
            sampleSize =
                16

            -- a full easing cycle is 0-1 over the period
            --  a delta sample is how big in the 0-1 space, is a 16ms chunk
            deltaSample =
                sampleSize / Duration.inMilliseconds period

            targetPixels =
                Pixels.pixels (ease target)

            prev =
                Pixels.pixels (ease (target - deltaSample))

            next =
                Pixels.pixels (ease (target + deltaSample))

            dx =
                Quantity.plus
                    (targetPixels |> Quantity.minus prev)
                    (next |> Quantity.minus targetPixels)
                    |> Quantity.divideBy 2
        in
        dx |> Quantity.per (Duration.milliseconds sampleSize)


type alias Pixels =
    Quantity.Quantity Float Pixels.Pixels


type alias PixelsPerSecond =
    Quantity.Quantity Float Pixels.PixelsPerSecond


velocityBetween : Pixels -> Time.Absolute -> Pixels -> Time.Absolute -> PixelsPerSecond
velocityBetween one oneTime two twoTime =
    let
        distance =
            one |> Quantity.minus two

        duration =
            Time.duration oneTime twoTime
    in
    distance |> Quantity.per duration


color : (event -> Color.Color) -> Timeline.Occurring event -> Maybe (Timeline.Occurring event) -> Timeline.Phase Color.Color -> Color.Color
color lookup (Timeline.Occurring target targetTime maybeDwell) maybeLookAhead phase =
    case phase of
        Timeline.Start ->
            lookup target

        Timeline.After state ->
            lookup target

        Timeline.TransitioningTo progress state ->
            let
                one =
                    Color.toRgba state

                two =
                    Color.toRgba (lookup target)
            in
            Color.rgba
                (average one.red two.red progress.percent)
                (average one.green two.green progress.percent)
                (average one.blue two.blue progress.percent)
                (average one.alpha two.alpha progress.percent)

        Timeline.Resting restingDuration state ->
            lookup target


average : Float -> Float -> Float -> Float
average x y progress =
    sqrt ((x * x) * (1 - progress) + (y * y) * progress)



-- f(t) = -1/2 e^(-6 t) (-2 e^(6 t) + sin(12 t) + 2 cos(12 t))
-- mix two easing functions given a weight and a percent (what point on the easing are we at)


mix fnA fnB weightB percent =
    let
        a =
            fnA percent

        b =
            fnB percent
    in
    a + weightB (b - a)
