module Internal.Interpolate exposing
    ( point, toPoint
    , color, linear
    , Motion, motion, toMotion
    , Movement(..), movement, toMovement
    , MotionMovement
    )

{-|

@docs point, toPoint

@docs color, linear

@docs Motion, motion, point, toMotion, toPoint

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


point : Timeline.Interpolator PointMotion
point one two progress =
    { x = motion one.x two.x progress
    , y = motion one.y two.y progress
    }


mapEventPair fn maybe =
    Maybe.map (Tuple.mapFirst fn) maybe


toPoint : Timeline.Promoter Point PointMotion
toPoint maybePrev current now maybeLookAhead =
    { x = toMotion (mapEventPair .x maybePrev) current.x now (mapEventPair .x maybeLookAhead)
    , y = toMotion (mapEventPair .y maybePrev) current.y now (mapEventPair .y maybeLookAhead)
    }


motion : Timeline.Interpolator Motion
motion p1 p2 t =
    let
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


toMotion : Timeline.Promoter Float Motion
toMotion maybePrev current targetTime maybeLookAhead =
    case maybePrev of
        Nothing ->
            -- maybePrev is Nothing when we're at the very first element.
            { position = current
            , velocity = 0
            , between = Nothing
            }

        Just ( prev, prevTime ) ->
            case maybeLookAhead of
                Nothing ->
                    { position = current
                    , velocity = 0
                    , between = Nothing
                    }

                Just ( ahead, aheadTime ) ->
                    { position = current
                    , velocity =
                        1000 * ((ahead - current) / Duration.inMilliseconds (Time.duration aheadTime targetTime))
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
        toFloat
            ((totalDuration |> modBy duration)
                // duration
            )


{-| Move between events
-}
movement : Timeline.Interpolator MotionMovement
movement one two t =
    let
        totalDuration =
            Quantity.multiplyBy t (Time.duration one.time two.time)

        p1 =
            case one.movement of
                Oscillate center duration toX ->
                    center + toX (wrapUnitAfter duration totalDuration)

                Position x ->
                    x

        p2 =
            case two.movement of
                Oscillate center duration toX ->
                    center + toX (wrapUnitAfter duration totalDuration)

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


toMovement : Timeline.Promoter Movement MotionMovement
toMovement maybePrev current targetTime maybeLookAhead =
    let
        anchorPosition =
            case current of
                Oscillate center _ toX ->
                    center + toX 0

                Position x ->
                    x
    in
    case maybePrev of
        Nothing ->
            -- maybePrev is Nothing when we're at the very first element.
            { position = anchorPosition
            , velocity = 0
            , movement = current
            , time = targetTime
            }

        Just ( prev, prevTime ) ->
            case maybeLookAhead of
                Nothing ->
                    { position = anchorPosition
                    , velocity = 0
                    , movement = current
                    , time = targetTime
                    }

                Just ( ahead, aheadTime ) ->
                    let
                        aheadPosition =
                            case current of
                                Oscillate center _ toX ->
                                    center + toX 0

                                Position x ->
                                    x
                    in
                    { position = anchorPosition
                    , velocity =
                        1000 * ((aheadPosition - anchorPosition) / Duration.inMilliseconds (Time.duration aheadTime targetTime))
                    , movement = current
                    , time = targetTime
                    }


{-| Sort of a weird one, but is the base case where no promotion is needed.
-}
linear : Timeline.Promoter thing thing
linear maybePrev current now maybeLookAhead =
    current


{-| -}
color : Timeline.Interpolator Color.Color
color colorOne colorTwo progress =
    let
        one =
            Color.toRgba colorOne

        two =
            Color.toRgba colorTwo
    in
    Color.rgba
        (average one.red two.red progress)
        (average one.green two.green progress)
        (average one.blue two.blue progress)
        (average one.alpha two.alpha progress)


average : Float -> Float -> Float -> Float
average x y progress =
    sqrt ((x ^ 2) * (1 - progress) + (y ^ 2) * progress)
