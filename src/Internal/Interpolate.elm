module Internal.Interpolate exposing
    ( color
    , Motion, motion, toMotion
    , Movement(..), movement, toMovement
    , MotionMovement, derivativeOfEasing, mobilize
    )

{-|

@docs point, toPoint

@docs color, linear

@docs Motion, motion, toMotion

@docs Movement, movement, toMovement

-}

import Color
import CubicSpline2d
import Duration
import Internal.Time as Time
import Internal.Timeline as Timeline
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


motion : Timeline.Interpolator Motion
motion p1 p2 progress =
    let
        t =
            progress

        curve =
            CubicSpline2d.fromEndpoints
                (Point2d.unitless 0 p1.position)
                (Vector2d.unitless 1 p1.velocity)
                (Point2d.unitless 1 p2.position)
                (Vector2d.unitless 1 p2.velocity)
    in
    { position =
        unwrapQuantity (Point2d.yCoordinate (CubicSpline2d.pointOn curve t))
    , velocity =
        unwrapQuantity (Vector2d.yComponent (CubicSpline2d.firstDerivative curve t))
    , between =
        Just
            ( { position = p1.position, velocity = p1.velocity }
            , { position = p2.position, velocity = p2.velocity }
            )
    }


toMotion : Timeline.Promoter event Float Motion
toMotion lookup maybePrev (Timeline.Occurring target targetTime maybeDwell) maybeLookAhead =
    let
        currentPos =
            lookup target
    in
    case maybePrev of
        Nothing ->
            -- maybePrev is Nothing when we're at the very first element.
            { position = currentPos
            , velocity = 0
            , between = Nothing
            }

        Just _ ->
            case maybeLookAhead of
                Nothing ->
                    { position = currentPos
                    , velocity = 0
                    , between = Nothing
                    }

                Just (Timeline.Occurring ahead aheadTime ahedDwell) ->
                    { position = currentPos
                    , velocity =
                        1000 * ((lookup ahead - currentPos) / Duration.inMilliseconds (Time.duration aheadTime targetTime))
                    , between = Nothing
                    }


type
    Movement
    -- oscillate around a point
    = Oscillate Float Time.Duration (Float -> Float)
    | Position Float


type alias MotionMovement =
    { movement : Movement
    , position : Float
    , velocity : Float
    , time : Time.Absolute
    }


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


{-| Move between events
-}
movement : Timeline.Interpolator MotionMovement
movement one two progress =
    -- if progress.eventsAreEqual then
    --     -- this means we're "resting" at event one
    --     let
    --         totalDuration =
    --             Quantity.multiplyBy progress.percent (Time.duration one.time two.time)
    --         pos =
    --             case one.movement of
    --                 Oscillate center duration toX ->
    --                     center + toX (wrapUnitAfter duration totalDuration)
    --                 Position x ->
    --                     x
    --         velocity =
    --             case one.movement of
    --                 Oscillate center duration toX ->
    --                     one.velocity
    --                 Position x ->
    --                     one.velocity
    --     in
    --     { position = pos
    --     , velocity = velocity
    --     , movement = one.movement
    --     , time = one.time
    --     }
    -- else
    let
        t =
            progress

        totalDuration =
            Quantity.multiplyBy t (Time.duration one.time two.time)

        p1 =
            case one.movement of
                Oscillate center duration toX ->
                    center

                -- + toX (wrapUnitAfter duration totalDuration)
                Position x ->
                    x

        p2 =
            case two.movement of
                Oscillate center duration toX ->
                    center

                --+ toX (wrapUnitAfter duration totalDuration)
                Position x ->
                    x

        v1 =
            case one.movement of
                Oscillate center duration toX ->
                    one.velocity

                Position x ->
                    one.velocity

        v2 =
            case two.movement of
                Oscillate center duration toX ->
                    one.velocity

                Position x ->
                    two.velocity
    in
    if p1 == p2 && v1 == v2 then
        -- avoid some involved math when possible
        { position = p1
        , velocity = v1
        , movement = one.movement
        , time = one.time
        }

    else
        let
            curve =
                CubicSpline2d.fromEndpoints
                    (Point2d.unitless 0 p1)
                    (Vector2d.unitless 1 v1)
                    (Point2d.unitless 1 p2)
                    (Vector2d.unitless 1 v2)
        in
        { position =
            unwrapQuantity (Point2d.yCoordinate (CubicSpline2d.pointOn curve t))
        , velocity =
            unwrapQuantity (Vector2d.yComponent (CubicSpline2d.firstDerivative curve t))
        , movement = one.movement
        , time = one.time
        }


toMovement : Timeline.Promoter event Movement MotionMovement
toMovement lookup maybePrev (Timeline.Occurring target targetTime targetDwell) maybeLookAhead =
    let
        anchorPosition =
            case lookup target of
                Oscillate center _ toX ->
                    center + toX 0

                Position x ->
                    x
    in
    -- case maybePrev of
    --     Nothing ->
    --         -- maybePrev is Nothing when we're at the very first element.
    --         { position = anchorPosition
    --         , velocity = 0
    --         , movement = lookup target
    --         , time = targetTime
    --         }
    --     Just ( prev, prevTime ) ->
    case maybeLookAhead of
        Nothing ->
            { position = anchorPosition
            , velocity = 0
            , movement = lookup target
            , time = targetTime
            }

        Just (Timeline.Occurring ahead aheadTime aheadDwell) ->
            let
                aheadPosition =
                    case lookup ahead of
                        Oscillate center _ toX ->
                            center + toX 0

                        Position x ->
                            x
            in
            { position = anchorPosition
            , velocity =
                1000 * ((aheadPosition - anchorPosition) / Duration.inMilliseconds (Time.duration aheadTime targetTime))
            , movement = lookup target
            , time = targetTime
            }


type alias Progress =
    Float


type alias State =
    { position : Float
    , velocity : Float
    }


{-|

    `phase` captures if we are looking for the state:
        - transitioning to the target event
        - or while the target event has been active

-}
mobilize : (event -> Movement) -> Timeline.Occurring event -> Maybe (Timeline.Occurring event) -> Timeline.Phase State -> State
mobilize lookup (Timeline.Occurring target targetTime maybeDwell) maybeLookAhead phase =
    let
        anchorPosition =
            case lookup target of
                Oscillate center _ toX ->
                    center + toX 0

                Position x ->
                    x
    in
    case phase of
        Timeline.Start ->
            { position = anchorPosition
            , velocity = 0
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
                                center + toX 0

                            Just dwell ->
                                center + toX (wrapUnitAfter period dwell)

                    Position x ->
                        x
            , velocity =
                case maybeDwell of
                    Nothing ->
                        case maybeLookAhead of
                            Nothing ->
                                0

                            Just (Timeline.Occurring lookAhead aheadTime maybeLookAheadDwell) ->
                                case lookup lookAhead of
                                    Oscillate center freq toX ->
                                        -- calc forward velocity?
                                        1000
                                            * ((center - anchorPosition)
                                                / Duration.inMilliseconds (Time.duration aheadTime targetTime)
                                              )

                                    Position aheadPosition ->
                                        -- we're not dwelling here, and we're moving on to `ahead
                                        -- 1000
                                        --     * ((aheadPosition - anchorPosition)
                                        --         / Duration.inMilliseconds (Time.duration aheadTime targetTime)
                                        --       )
                                        0

                    Just dwell ->
                        case lookup target of
                            Oscillate center period toX ->
                                -- TODO, calc velocity
                                derivativeOfEasing toX period (wrapUnitAfter period dwell)

                            Position aheadPosition ->
                                0

            -- case maybeLookAhead of
            --     Nothing ->
            --         -- we are dwelling here
            --         -- oscillator would have a velocity
            --         case lookup target of
            --             Oscillate center period toX ->
            --                 -- derivativeOfEasing toX period (wrapUnitAfter period dwell)
            --                 0
            --             Position aheadPosition ->
            --                 0
            --     Just (Timeline.Occurring lookAhead aheadTime maybeLookAheadDwell) ->
            --         case lookup lookAhead of
            --             Oscillate center period toX ->
            --                 -- TODO, calc velocity
            --                 -- derivativeOfEasing toX period 0
            --                 0
            --             Position aheadPosition ->
            --                 0
            }

        Timeline.TransitioningTo progress state ->
            let
                -- at end of transition
                -- we don't care about dwell time
                -- because we're not there yet.
                velocityAtEndOfTransition =
                    case maybeLookAhead of
                        Nothing ->
                            0

                        Just (Timeline.Occurring lookAhead aheadTime maybeLookAheadDwell) ->
                            case lookup target of
                                Oscillate center period toX ->
                                    derivativeOfEasing toX period 0

                                Position aheadPosition ->
                                    1000
                                        * ((aheadPosition - anchorPosition)
                                            / Duration.inMilliseconds (Time.duration aheadTime targetTime)
                                          )

                curve =
                    CubicSpline2d.fromEndpoints
                        (Point2d.unitless 0 state.position)
                        (Vector2d.unitless 1 state.velocity)
                        (Point2d.unitless 1 anchorPosition)
                        (Vector2d.unitless 1 velocityAtEndOfTransition)
            in
            { position =
                unwrapQuantity
                    (Point2d.yCoordinate
                        (CubicSpline2d.pointOn curve progress)
                    )
            , velocity =
                unwrapQuantity
                    (Vector2d.yComponent
                        (CubicSpline2d.firstDerivative curve progress)
                    )
            }

        Timeline.Resting restingDuration state ->
            case lookup target of
                Position pos ->
                    { position = pos
                    , velocity = 0
                    }

                Oscillate center period toX ->
                    { position = center + toX (wrapUnitAfter period restingDuration)

                    -- TODO, calculate velocity estimation for oscillator
                    , velocity = derivativeOfEasing toX period (wrapUnitAfter period restingDuration)
                    }


{-|

    `ease` -> easing function of {0:1} -> value
    `period` -> how long is one cycle?  We're only looking at one cycle, so this could also be named "totalDuration"
    `target` -> Where do we want to calculate a derivative?

In order to calcualte the derivative, we take 3 evenly spaced samples, spaced 16ms apart.
16ms seems like a good choice because if the period is faster than that, the position will simply skip around randomly and not be smooth.

-}
derivativeOfEasing ease period target =
    if Duration.inMilliseconds period < 48 then
        0

    else
        let
            -- a full easing cycle is 0-1 over the period
            --  a delta sample is how big in the 0-1 space, is a 16ms chunk
            deltaSample =
                16 / Duration.inMilliseconds period

            dx =
                if target + (2 * deltaSample) > 1 then
                    -- target is too close to end, calculate from behind
                    approaching
                        (ease (target - deltaSample) - ease target)
                        (ease (target - (2 * deltaSample)) - ease (target - deltaSample))

                else if target - (2 * deltaSample) < 0 then
                    -- target is too close to beginning
                    approaching
                        (ease (target + deltaSample) - ease target)
                        (ease (target + (2 * deltaSample)) - ease (target + deltaSample))

                else
                    -- or take the average of one sample before and one after
                    0
        in
        dx / deltaSample


approaching d1 d2 =
    let
        ddx =
            d1 - d2

        d0 =
            d1 + ddx
    in
    avg d0 d1


avg one two =
    (one + two) / 2


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
                (average one.red two.red progress)
                (average one.green two.green progress)
                (average one.blue two.blue progress)
                (average one.alpha two.alpha progress)

        Timeline.Resting restingDuration state ->
            lookup target


average : Float -> Float -> Float -> Float
average x y progress =
    sqrt ((x ^ 2) * (1 - progress) + (y ^ 2) * progress)
