module Internal.Interpolate exposing
    ( color
    , Movement(..), move, xy, xyz
    , derivativeOfEasing
    , adjustTiming, defaultArrival, defaultDeparture, startColoring, startMoving, startMovingXy, startMovingXyz
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


{-|

    oscillate around a point

    or go to a specific position

-}
type Movement
    = Oscillate Departure Arrival Time.Duration (Float -> Float)
    | Position Departure Arrival Float


defaultDeparture : Departure
defaultDeparture =
    { late = 0
    , slowly = 0.4
    }


defaultArrival : Arrival
defaultArrival =
    { wobbliness = 0
    , early = 0
    , slowly = 0.8
    }


nullDeparture : Departure
nullDeparture =
    { late = 0
    , slowly = 0
    }


nullArrival : Arrival
nullArrival =
    { wobbliness = 0
    , early = 0
    , slowly = 0
    }


{-| Number betwen 0 and 1
-}
type alias Proportion =
    Float


type alias Arrival =
    { wobbliness : Proportion
    , early : Proportion
    , slowly : Proportion
    }


type alias Departure =
    { late : Proportion
    , slowly : Proportion
    }


wrapUnitAfter : Duration.Duration -> Duration.Duration -> Float
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


startMovingXy : (event -> XY Movement) -> Timeline.Occurring event -> XY State
startMovingXy lookup start =
    { x = startMoving (lookup >> .x) start
    , y = startMoving (lookup >> .y) start
    }


xy : (event -> XY Movement) -> Timeline.Occurring event -> Timeline.Occurring event -> Maybe (Timeline.Occurring event) -> Timeline.Phase -> Time.Absolute -> XY State -> XY State
xy lookup prev current maybeLookAhead phase now state =
    { x = move (lookup >> .x) prev current maybeLookAhead phase now state.x
    , y = move (lookup >> .y) prev current maybeLookAhead phase now state.y
    }


type alias XYZ thing =
    { x : thing
    , y : thing
    , z : thing
    }


startMovingXyz : (event -> XYZ Movement) -> Timeline.Occurring event -> XYZ State
startMovingXyz lookup start =
    { x = startMoving (lookup >> .x) start
    , y = startMoving (lookup >> .y) start
    , z = startMoving (lookup >> .z) start
    }


xyz : (event -> XYZ Movement) -> Timeline.Occurring event -> Timeline.Occurring event -> Maybe (Timeline.Occurring event) -> Timeline.Phase -> Time.Absolute -> XYZ State -> XYZ State
xyz lookup prev current maybeLookAhead phase now state =
    { x = move (lookup >> .x) prev current maybeLookAhead phase now state.x
    , y = move (lookup >> .y) prev current maybeLookAhead phase now state.y
    , z = move (lookup >> .z) prev current maybeLookAhead phase now state.z
    }


{-| We need some way to start our iterating over the timeline.

To make writing the interpolation function easier, it's also nice to seed with an initial event.

We always want a previous event, a current target, and a maybe lookahead.

Currently `previous` is used to calculate last minute timing adjustments for interpolation.

i.e. arrive.late

-}
startMoving : (event -> Movement) -> Timeline.Occurring event -> State
startMoving lookup (Timeline.Occurring start startTime _) =
    let
        initPos =
            case lookup start of
                Oscillate _ _ _ toX ->
                    toX 0

                Position depart arrive x ->
                    x
    in
    { position = Pixels.pixels initPos
    , velocity = Pixels.pixelsPerSecond 0
    }


adjustTiming : Movement -> Timeline.Adjustment
adjustTiming moving =
    case moving of
        Oscillate departure arrival _ _ ->
            { arrivingEarly = arrival.early
            , leavingLate = departure.late
            }

        Position departure arrival _ ->
            { arrivingEarly = arrival.early
            , leavingLate = departure.late
            }


zeroDuration : Duration.Duration
zeroDuration =
    Duration.milliseconds 0


{-| -}
move : (event -> Movement) -> Timeline.Occurring event -> Timeline.Occurring event -> Maybe (Timeline.Occurring event) -> Timeline.Phase -> Time.Absolute -> State -> State
move lookup previous target maybeLookAhead phase now state =
    case phase of
        Timeline.Start ->
            -- because we're starting,
            -- we know that `now` is *after* targetTime
            -- also, we don't care about `previous`
            let
                endTime =
                    case maybeLookAhead of
                        Nothing ->
                            now

                        Just lookAhead ->
                            Time.earliest
                                (Timeline.endTime target)
                                now
            in
            dwellFor (lookup (Timeline.getEvent target))
                (Time.duration (Timeline.startTime target) endTime)

        Timeline.Transitioning ->
            -- we're somewhere between prev and target.
            if Time.thisBeforeThat now (Timeline.endTime previous) then
                -- we're dwelling at prev
                -- dwell till `now`
                let
                    endTime =
                        Time.earliest
                            (Timeline.endTime previous)
                            now
                in
                dwellFor (lookup (Timeline.getEvent previous))
                    (Time.duration (Timeline.startTime previous) endTime)

            else if Time.thisBeforeThat now (Timeline.startTime target) then
                -- we're transitioning between 1 and 2
                -- dwell till `transitionStart`
                -- then transition till now
                -- with a target velocity based on lookahead
                interpolateBetween lookup previous target maybeLookAhead now state

            else if Timeline.hasDwell target then
                dwellFor (lookup (Timeline.getEvent target))
                    (Time.duration
                        (Timeline.startTime target)
                        (Time.earliest now (Timeline.endTime target))
                    )

            else if maybeLookAhead == Nothing then
                dwellFor (lookup (Timeline.getEvent target))
                    (Time.duration
                        (Timeline.startTime target)
                        now
                    )

            else
                { position =
                    case lookup (Timeline.getEvent target) of
                        Oscillate depart arrive period toX ->
                            Pixels.pixels (toX 0)

                        Position _ _ x ->
                            Pixels.pixels x
                , velocity =
                    velocityAtTarget lookup target maybeLookAhead
                }


dwellFor : Movement -> Time.Duration -> State
dwellFor movement duration =
    case movement of
        Position _ _ pos ->
            { position = Pixels.pixels pos
            , velocity = Pixels.pixelsPerSecond 0
            }

        Oscillate _ _ period toX ->
            { position = Pixels.pixels (toX (wrapUnitAfter period duration))
            , velocity = derivativeOfEasing toX period (wrapUnitAfter period duration)
            }


interpolateBetween : (event -> Movement) -> Timeline.Occurring event -> Timeline.Occurring event -> Maybe (Timeline.Occurring event) -> Time.Absolute -> State -> State
interpolateBetween lookup previous ((Timeline.Occurring target targetTime maybeTargetDwell) as targetOccurring) maybeLookAhead currentTime state =
    let
        targetPosition =
            case lookup target of
                Oscillate _ _ _ toX ->
                    Pixels.pixels (toX 0)

                Position _ _ x ->
                    Pixels.pixels x

        targetTimeInMs =
            Time.inMilliseconds targetTime

        startTimeInMs =
            Time.inMilliseconds (Timeline.endTime previous)

        targetVelocity =
            Pixels.inPixelsPerSecond (velocityAtTarget lookup targetOccurring maybeLookAhead)

        curve =
            createSpline
                { start = Point2d.unitless startTimeInMs (Pixels.inPixels state.position)
                , startVelocity = Vector2d.unitless 1000 (Pixels.inPixelsPerSecond state.velocity)
                , departure = getLeave lookup previous
                , end = Point2d.unitless targetTimeInMs (Pixels.inPixels targetPosition)
                , endVelocity =
                    Vector2d.unitless 1000
                        targetVelocity
                , arrival = getArrival lookup targetOccurring
                }

        current =
            findAtX curve
                (Time.inMilliseconds currentTime)
                -- tolerance
                1
                -- jumpSize
                0.25
                -- starting t
                0.5
                -- depth
                0

        firstDerivative =
            CubicSpline2d.firstDerivative curve current.t
    in
    { position =
        current.point
            |> Point2d.yCoordinate
            |> Quantity.toFloat
            |> Pixels.pixels
    , velocity =
        -- rescale velocity so that it's pixels/second
        -- `createSpline` scales this vector sometimes, we need to ensure it's the right size.
        (Quantity.toFloat (Vector2d.yComponent firstDerivative)
            / Quantity.toFloat (Vector2d.xComponent firstDerivative)
        )
            |> (*) 1000
            |> Pixels.pixelsPerSecond
    }


velocityAtTarget : (event -> Movement) -> Timeline.Occurring event -> Maybe (Timeline.Occurring event) -> PixelsPerSecond
velocityAtTarget lookup (Timeline.Occurring target targetTime maybeTargetDwell) maybeLookAhead =
    -- This is the velocity we're shooting for.
    case maybeTargetDwell of
        Nothing ->
            case maybeLookAhead of
                Nothing ->
                    case lookup target of
                        Position _ _ _ ->
                            Pixels.pixelsPerSecond 0

                        Oscillate _ _ period toX ->
                            -- if there's no dwell and no lookahead,
                            -- then we're approaching the last event
                            -- which will "dwell" automatically
                            -- until something happens
                            derivativeOfEasing toX period 0

                Just (Timeline.Occurring lookAhead aheadTime maybeLookAheadDwell) ->
                    let
                        targetPosition =
                            case lookup target of
                                Oscillate _ _ _ toX ->
                                    Pixels.pixels (toX 0)

                                Position _ _ x ->
                                    Pixels.pixels x
                    in
                    case lookup lookAhead of
                        Position _ _ aheadPosition ->
                            -- our target velocity is the linear velocity between target and lookahead
                            velocityBetween targetPosition targetTime (Pixels.pixels aheadPosition) aheadTime

                        Oscillate _ _ period toX ->
                            case maybeLookAheadDwell of
                                Nothing ->
                                    velocityBetween targetPosition targetTime (Pixels.pixels (toX 0)) aheadTime

                                Just _ ->
                                    derivativeOfEasing toX period 0

        Just dwell ->
            case lookup target of
                Position _ _ _ ->
                    Pixels.pixelsPerSecond 0

                Oscillate _ _ period toX ->
                    derivativeOfEasing toX period 0


getLeave lookup (Timeline.Occurring event _ _) =
    case lookup event of
        Position departure _ _ ->
            departure

        Oscillate departure _ _ _ ->
            departure


getArrival lookup (Timeline.Occurring event _ _) =
    case lookup event of
        Position _ arrival _ ->
            arrival

        Oscillate _ arrival _ _ ->
            arrival


type alias Curve coordinates =
    { start : Point2d.Point2d Quantity.Unitless coordinates
    , startVelocity : Vector2d.Vector2d Quantity.Unitless coordinates
    , departure : Departure
    , end : Point2d.Point2d Quantity.Unitless coordinates
    , endVelocity : Vector2d.Vector2d Quantity.Unitless coordinates
    , arrival : Arrival
    }


createSpline : Curve coordinates -> CubicSpline2d.CubicSpline2d Quantity.Unitless coordinates
createSpline config =
    let
        startX =
            config.start
                |> Point2d.xCoordinate
                |> Quantity.toFloat

        endX =
            config.end
                |> Point2d.xCoordinate
                |> Quantity.toFloat

        totalX =
            endX - startX

        startVelocity =
            if config.departure.slowly == 0 then
                config.startVelocity

            else if config.startVelocity == Vector2d.zero then
                Vector2d.unitless totalX 0
                    |> Vector2d.scaleBy (config.departure.slowly * 3)

            else
                config.startVelocity
                    |> Vector2d.scaleBy (config.departure.slowly * 3)

        endVelocity =
            if config.arrival.slowly == 0 then
                config.endVelocity

            else if config.endVelocity == Vector2d.zero then
                Vector2d.unitless totalX 0
                    |> Vector2d.scaleBy (config.arrival.slowly * 3)

            else
                config.endVelocity
                    |> Vector2d.scaleBy (config.arrival.slowly * 3)
    in
    CubicSpline2d.fromEndpoints
        config.start
        startVelocity
        config.end
        endVelocity


{-| Once we have a bezier curve, we need to find the value of y at a given x.

A simple way to do this is just a binary search, which is what this does.

However we could use Newton's Method:
<https://en.wikipedia.org/wiki/Newton%27s_method>
<http://greweb.me/2012/02/bezier-curve-based-easing-functions-from-concept-to-implementation/>

OR (and I'm not 100% on this one), we could use Cardano's method:

as explained here:
<https://stackoverflow.com/questions/51879836/cubic-bezier-curves-get-y-for-given-x-special-case-where-x-of-control-points/51883347#51883347>

-}



--findAtX : CubicSpline2d.CubicSpline2d Pixels.Pixels Pixels.Pixels -> Float -> Float -> Float -> Float -> Float -> { x : Float, y : Float }


findAtX spline desiredX tolerance jumpSize t depth =
    let
        point =
            CubicSpline2d.pointOn spline t

        p =
            point
                |> Point2d.toUnitless
    in
    if depth == 10 then
        { point = point
        , t = t
        }

    else if within tolerance p.x desiredX then
        { point = point
        , t = t
        }

    else if p.x > desiredX then
        findAtX spline desiredX tolerance (jumpSize / 2) (t - jumpSize) (depth + 1)

    else
        findAtX spline desiredX tolerance (jumpSize / 2) (t + jumpSize) (depth + 1)


within : Float -> Float -> Float -> Bool
within tolerance anchor at =
    let
        low =
            anchor - tolerance

        high =
            anchor + tolerance
    in
    at >= low && at <= high


bezier : Float -> Float -> Float -> Float -> Float -> Float
bezier x1 y1 x2 y2 time =
    let
        lerp from to v =
            from + (to - from) * v

        pair interpolate ( a0, b0 ) ( a1, b1 ) v =
            ( interpolate a0 a1 v, interpolate b0 b1 v )

        casteljau ps =
            case ps of
                [ ( x, y ) ] ->
                    y

                xs ->
                    List.map2 (\x y -> pair lerp x y time) xs (Maybe.withDefault [] (List.tail xs))
                        |> casteljau
    in
    casteljau [ ( 0, 0 ), ( x1, y1 ), ( x2, y2 ), ( 1, 1 ) ]


linear start end percent =
    let
        dur =
            end - start
    in
    start + (percent * dur)


{-|

    `ease` -> easing function of {0:1} -> value
    `period` -> how long is one cycle?  We're only looking at one cycle, so this could also be named "totalDuration"
    `target` -> Where do we want to calculate a derivative?

In order to calculate the derivative, we take 3 evenly spaced samples, spaced 16ms apart.
16ms seems like a good choice because if the period is faster than that, the position will simply skip around randomly and not be smooth.

-}
derivativeOfEasing : (Float -> Float) -> Time.Duration -> Float -> PixelsPerSecond
derivativeOfEasing ease period target =
    let
        sampleSize =
            16

        -- a full easing cycle is 0-1 over the period
        -- a delta sample is how big in the 0-1 space, is a 16ms chunk
        deltaSample =
            sampleSize
                / Duration.inMilliseconds period

        targetPixels =
            Pixels.pixels (ease target)

        prev =
            Pixels.pixels (ease (target - deltaSample))

        next =
            Pixels.pixels (ease (target + deltaSample))

        dx1 =
            targetPixels |> Quantity.minus prev

        dx2 =
            next |> Quantity.minus targetPixels

        dx =
            Quantity.plus dx1 dx2
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
            two |> Quantity.minus one

        duration =
            Time.duration oneTime twoTime
    in
    distance |> Quantity.per duration


startColoring : (event -> Color.Color) -> Timeline.Occurring event -> Color.Color
startColoring lookup (Timeline.Occurring start startTime _) =
    lookup start


color : (event -> Color.Color) -> Timeline.Occurring event -> Timeline.Occurring event -> Maybe (Timeline.Occurring event) -> Timeline.Phase -> Time.Absolute -> Color.Color -> Color.Color
color lookup previous ((Timeline.Occurring target targetTime maybeDwell) as targetOccurring) maybeLookAhead phase now state =
    case phase of
        Timeline.Start ->
            lookup target

        Timeline.Transitioning ->
            let
                eventEndTime =
                    Timeline.endTime targetOccurring
            in
            if Time.thisAfterThat now eventEndTime || Time.thisAfterThat now targetTime then
                lookup target

            else
                let
                    progress =
                        Time.progress
                            (Timeline.endTime previous)
                            targetTime
                            now

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
