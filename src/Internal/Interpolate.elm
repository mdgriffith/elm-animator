module Internal.Interpolate exposing
    ( color
    , Movement(..), move
    , derivativeOfEasing
    )

{-|

@docs color

@docs Movement, move

@docs derivativeOfEasing

-}

import Color
import CubicSpline2d
import Duration
import Internal.Time as Time
import Internal.Timeline as Timeline
import Pixels
import Point2d
import Quantity
import Vector2d


type alias Motion =
    { position : Float
    , velocity : Float
    , between : Maybe ( P, P )
    }


type alias P =
    { position : Float
    , velocity : Float
    }


unwrapQuantity (Quantity.Quantity value) =
    value


type alias Point =
    { x : Float
    , y : Float
    }


type alias PointMotion =
    { x : Motion
    , y : Motion
    }



-- point : Timeline.Interpolator PointMotion
-- point one two progress =
--     { x = motion one.x two.x progress
--     , y = motion one.y two.y progress
--     }
-- mapEventPair fn maybe =
--     Maybe.map (Tuple.mapFirst fn) maybe
-- toPoint : Timeline.Promoter event Point PointMotion
-- toPoint lookup maybePrev current now maybeLookAhead =
--     { x = toMotion (mapEventPair .x maybePrev) current.x now (mapEventPair .x maybeLookAhead)
--     , y = toMotion (mapEventPair .y maybePrev) current.y now (mapEventPair .y maybeLookAhead)
--     }
-- oscillate around a point
-- or go to a specific position


type Movement
    = Oscillate Float Time.Duration (Float -> Float)
    | Position Float


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
                Oscillate center _ toX ->
                    Pixels.pixels (center + toX 0)

                Position x ->
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
                    Oscillate center period toX ->
                        case maybeDwell of
                            Nothing ->
                                -- we havent had time to oscillate if there is no dwell time.
                                Pixels.pixels (center + toX 0)

                            Just dwell ->
                                Pixels.pixels (center + toX (wrapUnitAfter period dwell))

                    Position x ->
                        Pixels.pixels x
            , velocity =
                case maybeDwell of
                    Nothing ->
                        case maybeLookAhead of
                            Nothing ->
                                Pixels.pixelsPerSecond 0

                            Just (Timeline.Occurring lookAhead aheadTime maybeLookAheadDwell) ->
                                case lookup lookAhead of
                                    Oscillate center freq toX ->
                                        -- calc forward velocity?
                                        velocityBetween targetPosition targetTime (Pixels.pixels center) aheadTime

                                    Position aheadPosition ->
                                        -- we're not dwelling here, and we're moving on to `ahead
                                        velocityBetween targetPosition targetTime (Pixels.pixels aheadPosition) aheadTime

                    Just dwell ->
                        case lookup target of
                            Oscillate center period toX ->
                                derivativeOfEasing toX period (wrapUnitAfter period dwell)

                            Position aheadPosition ->
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
                            Pixels.pixelsPerSecond 0

                        Just (Timeline.Occurring lookAhead aheadTime maybeLookAheadDwell) ->
                            case lookup target of
                                Oscillate center period toX ->
                                    derivativeOfEasing toX period 0

                                Position aheadPosition ->
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
                Position pos ->
                    { position = Pixels.pixels pos
                    , velocity = Pixels.pixelsPerSecond 0
                    }

                Oscillate center period toX ->
                    { position = Pixels.pixels (center + toX (wrapUnitAfter period restingDuration))
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
    sqrt ((x ^ 2) * (1 - progress) + (y ^ 2) * progress)



-- f(t) = -1/2 e^(-6 t) (-2 e^(6 t) + sin(12 t) + 2 cos(12 t))
