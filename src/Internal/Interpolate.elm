module Internal.Interpolate exposing
    ( color
    , Movement(..), move, xy, xyz
    , derivativeOfEasing
    , adjustTiming, defaultArrival, defaultDeparture, pass, startColoring, startMoving, startMovingXy, startMovingXyz, startPass
    )

{-|

@docs color

@docs Movement, move, xy, xyz

@docs derivativeOfEasing

-}

import Color
import Duration
import Internal.Spring as Spring
import Internal.Time as Time
import Internal.Timeline as Timeline
import Pixels
import Quantity


startPass : (event -> event) -> Timeline.Occurring event -> event
startPass lookup (Timeline.Occurring start startTime _) =
    start


pass : (event -> event) -> Timeline.Occurring event -> Timeline.Occurring event -> Maybe (Timeline.Occurring event) -> Timeline.Phase -> Time.Absolute -> event -> event
pass _ _ target _ _ _ _ =
    Timeline.getEvent target


{-|

    oscillate around a point

    or go to a specific position

-}
type Movement
    = Oscillate Departure Arrival Time.Duration (Float -> Float)
    | Position Departure Arrival Float


{-| Number betwen 0 and 1
-}
type alias Proportion =
    Float


{-| Arrival parameters:

  - early [0-1]:
    how early should we arrive to this state?
    This is a Proportion of the entire duration of the transition.

  - slowly [0-1]: actually smoothness.
    How far do we scale the bezier curve. 0, nothing, 1 == full length of curve.

-}
type alias Arrival =
    { wobbliness : Proportion
    , early : Proportion
    , slowly : Proportion
    }


type alias Departure =
    { late : Proportion
    , slowly : Proportion
    }


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


log str val =
    val



-- Debug.log
--     str
--     val


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
            if log "dwell-prev" <| Time.thisBeforeThat now (Timeline.endTime previous) then
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

            else if log "lerp" <| Time.thisBeforeThat now (Timeline.startTime target) then
                -- we're transitioning between 1 and 2
                -- transition till now
                -- with a target velocity based on lookahead
                let
                    wobble =
                        case lookup (Timeline.getEvent target) of
                            Oscillate _ arrival _ _ ->
                                arrival.wobbliness

                            Position _ arrival _ ->
                                arrival.wobbliness
                in
                if maybeLookAhead == Nothing && wobble /= 0 then
                    springInterpolation lookup previous target now state

                else
                    interpolateBetween lookup previous target maybeLookAhead now state

            else if log "dwell - target" <| Timeline.hasDwell target then
                dwellFor (lookup (Timeline.getEvent target))
                    (Time.duration
                        (Timeline.startTime target)
                        (Time.earliest now (Timeline.endTime target))
                    )

            else if log "auto dwell" <| maybeLookAhead == Nothing then
                dwellFor (lookup (Timeline.getEvent target))
                    (Time.duration
                        (Timeline.startTime target)
                        now
                    )

            else
                let
                    _ =
                        log "after" target
                in
                log "after target"
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


{-| -}
springInterpolation : (event -> Movement) -> Timeline.Occurring event -> Timeline.Occurring event -> Time.Absolute -> State -> State
springInterpolation lookup previous target now state =
    let
        wobble =
            case lookup (Timeline.getEvent target) of
                Oscillate _ arrival _ _ ->
                    arrival.wobbliness

                Position _ arrival _ ->
                    arrival.wobbliness

        targetPos =
            case lookup (Timeline.getEvent target) of
                Oscillate _ _ _ toX ->
                    toX 0

                Position _ _ x ->
                    x

        duration =
            Time.duration (Timeline.endTime previous) (Timeline.startTime target)

        params =
            Spring.select wobble duration

        new =
            Spring.stepOver
                (Time.duration (Timeline.endTime previous) now)
                params
                targetPos
                { position = Pixels.inPixels state.position
                , velocity = Pixels.inPixelsPerSecond state.velocity
                }
    in
    { position = Pixels.pixels new.position
    , velocity = Pixels.pixelsPerSecond new.velocity
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
                { start =
                    { x = startTimeInMs
                    , y = Pixels.inPixels state.position
                    }
                , startVelocity =
                    { x = 1000
                    , y = Pixels.inPixelsPerSecond state.velocity
                    }
                , departure = getLeave lookup previous
                , end =
                    { x = targetTimeInMs
                    , y = Pixels.inPixels targetPosition
                    }
                , endVelocity =
                    { x = 1000
                    , y = targetVelocity
                    }
                , arrival = getArrival lookup targetOccurring
                }

        current =
            findAtXOnSpline curve
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
            firstDerivativeOnSpline curve current.t
    in
    { position =
        current.point.y
            |> Pixels.pixels
    , velocity =
        -- rescale velocity so that it's pixels/second
        -- `createSpline` scales this vector sometimes, we need to ensure it's the right size.
        (firstDerivative.y / firstDerivative.x)
            |> (*) 1000
            |> Pixels.pixelsPerSecond
    }


velocityAtTarget : (event -> Movement) -> Timeline.Occurring event -> Maybe (Timeline.Occurring event) -> PixelsPerSecond
velocityAtTarget lookup ((Timeline.Occurring target targetTime maybeTargetDwell) as t) maybeLookAhead =
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


createSpline :
    { start : Point
    , startVelocity : Point
    , departure : Departure
    , end : Point
    , endVelocity : Point
    , arrival : Arrival
    }
    -> Spline
createSpline config =
    let
        totalX =
            config.end.x - config.start.x

        startVelocity =
            if config.departure.slowly == 0 then
                config.startVelocity

            else if config.startVelocity == zeroPoint then
                scaleBy (config.departure.slowly * 3)
                    { x = totalX
                    , y = 0
                    }

            else
                config.startVelocity
                    |> scaleBy (config.departure.slowly * 3)

        endVelocity =
            if config.arrival.slowly == 0 then
                config.endVelocity

            else if config.endVelocity == zeroPoint then
                scaleBy (config.arrival.slowly * 3)
                    { x = totalX
                    , y = 0
                    }

            else
                config.endVelocity
                    |> scaleBy (config.arrival.slowly * 3)
    in
    {-
       the `fromEndpoints` definition from elm-geometry

       fromControlPoints
               givenStartPoint
               (givenStartPoint |> Point2d.translateBy (Vector2d.scaleBy (1 / 3) givenStartDerivative))
               (givenEndPoint |> Point2d.translateBy (Vector2d.scaleBy (-1 / 3) givenEndDerivative))
               givenEndPoint


    -}
    Spline
        config.start
        (config.start |> translateBy (scaleBy (1 / 3) startVelocity))
        (config.end |> translateBy (scaleBy (-1 / 3) endVelocity))
        config.end


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



{- A mini embedded elm-geometry because I didn't want to impose it as a dependency.

   Though definitely worth checking out.

   https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/3.1.0/

   Thanks Ian!

-}


type Spline
    = Spline Point Point Point Point


zeroPoint : Point
zeroPoint =
    { x = 0
    , y = 0
    }


type alias Point =
    { x : Float
    , y : Float
    }


scaleBy : Float -> Point -> Point
scaleBy n { x, y } =
    { x = x * n
    , y = y * n
    }


translateBy : Point -> Point -> Point
translateBy delta { x, y } =
    { x = x + delta.x
    , y = y + delta.y
    }


interpolatePoints : Point -> Point -> Float -> Point
interpolatePoints p1 p2 t =
    if t <= 0.5 then
        { x = p1.x + t * (p2.x - p1.x)
        , y = p1.y + t * (p2.y - p1.y)
        }

    else
        { x = p2.x + (1 - t) * (p1.x - p2.x)
        , y = p2.y + (1 - t) * (p1.y - p2.y)
        }


interpolateValue : Float -> Float -> Float -> Float
interpolateValue start end t =
    if t <= 0.5 then
        start + t * (end - start)

    else
        end + (1 - t) * (start - end)


{-| Borrowed from: <https://github.com/ianmackenzie/elm-geometry/blob/3.1.0/src/CubicSpline2d.elm#L370>
-}
pointOn : Spline -> Proportion -> Point
pointOn (Spline p1 p2 p3 p4) proportion =
    let
        q1 =
            interpolatePoints p1 p2 proportion

        q2 =
            interpolatePoints p2 p3 proportion

        q3 =
            interpolatePoints p3 p4 proportion

        r1 =
            interpolatePoints q1 q2 proportion

        r2 =
            interpolatePoints q2 q3 proportion
    in
    interpolatePoints r1 r2 proportion


{-| Borrowed from: <https://github.com/ianmackenzie/elm-geometry/blob/3.1.0/src/CubicSpline2d.elm#L778>
-}
firstDerivativeOnSpline : Spline -> Proportion -> Point
firstDerivativeOnSpline (Spline p1 p2 p3 p4) proportion =
    let
        vx1 =
            p2.x - p1.x

        vy1 =
            p2.y - p1.y

        vx2 =
            p3.x - p2.x

        vy2 =
            p3.y - p2.y

        vx3 =
            p4.x - p3.x

        vy3 =
            p4.y - p3.y

        wx1 =
            interpolateValue vx1 vx2 proportion

        wy1 =
            interpolateValue vy1 vy2 proportion

        wx2 =
            interpolateValue vx2 vx3 proportion

        wy2 =
            interpolateValue vy2 vy3 proportion
    in
    { x =
        3 * interpolateValue wx1 wx2 proportion
    , y =
        3 * interpolateValue wy1 wy2 proportion
    }


{-| Borrowed from: <https://github.com/ianmackenzie/elm-geometry/blob/3.1.0/src/CubicSpline2d.elm#L858>
-}
secondDerivativeOnSpline : Spline -> Proportion -> Point
secondDerivativeOnSpline (Spline p1 p2 p3 p4) proportion =
    let
        u1 =
            { x = p2.x - p1.x
            , y = p2.y - p1.y
            }

        u2 =
            { x = p3.x - p2.x
            , y = p3.y - p2.y
            }

        u3 =
            { x = p4.x - p3.x
            , y = p4.y - p3.y
            }

        v1 =
            { x = u2.x - u1.x
            , y = u2.y - u1.y
            }

        v2 =
            { x = u3.x - u2.x
            , y = u3.y - u2.y
            }
    in
    scaleBy 6 (interpolatePoints v1 v2 proportion)


{-| Once we have a bezier curve, we need to find the value of y at a given x.

A simple way to do this is just a binary search, which is what this does.

However we could use Newton's Method:
<https://en.wikipedia.org/wiki/Newton%27s_method>
<http://greweb.me/2012/02/bezier-curve-based-easing-functions-from-concept-to-implementation/>

OR (and I'm not 100% on this one), we could use Cardano's method:

as explained here:
<https://stackoverflow.com/questions/51879836/cubic-bezier-curves-get-y-for-given-x-special-case-where-x-of-control-points/51883347#51883347>

-}
findAtXOnSpline : Spline -> Float -> Float -> Float -> Float -> Int -> { point : { x : Float, y : Float }, t : Float }
findAtXOnSpline spline desiredX tolerance jumpSize t depth =
    let
        point =
            pointOn spline t
    in
    if depth == 10 then
        { point = point
        , t = t
        }

    else if within tolerance point.x desiredX then
        { point = point
        , t = t
        }

    else if point.x > desiredX then
        findAtXOnSpline spline desiredX tolerance (jumpSize / 2) (t - jumpSize) (depth + 1)

    else
        findAtXOnSpline spline desiredX tolerance (jumpSize / 2) (t + jumpSize) (depth + 1)
