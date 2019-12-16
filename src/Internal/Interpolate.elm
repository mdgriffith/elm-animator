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
    , slowly = 0
    }


defaultArrival : Arrival
defaultArrival =
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


xy : (event -> XY Movement) -> Timeline.Occurring event -> Maybe (Timeline.Occurring event) -> Timeline.Phase event (XY State) -> XY State
xy lookup current maybeLookAhead phase =
    { x = move (lookup >> .x) current maybeLookAhead (Timeline.mapPhase .x phase)
    , y = move (lookup >> .y) current maybeLookAhead (Timeline.mapPhase .y phase)
    }


type alias XYZ thing =
    { x : thing
    , y : thing
    , z : thing
    }


xyz : (event -> XYZ Movement) -> Timeline.Occurring event -> Maybe (Timeline.Occurring event) -> Timeline.Phase event (XYZ State) -> XYZ State
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
move : (event -> Movement) -> Timeline.Occurring event -> Maybe (Timeline.Occurring event) -> Timeline.Phase event State -> State
move lookup ((Timeline.Occurring target targetTime maybeDwell) as targetOccurring) maybeLookAhead phase =
    let
        targetPosition =
            case lookup target of
                Oscillate _ _ _ toX ->
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
                    Oscillate depart arrive period toX ->
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
                                    Oscillate depart arrive freq toX ->
                                        -- calc forward velocity?
                                        velocityBetween targetPosition targetTime (Pixels.pixels (toX 0)) aheadTime

                                    Position _ _ aheadPosition ->
                                        -- we're not dwelling here, and we're moving on to `ahead
                                        velocityBetween targetPosition targetTime (Pixels.pixels aheadPosition) aheadTime

                    Just dwell ->
                        case lookup target of
                            Oscillate depart arrive period toX ->
                                derivativeOfEasing toX period (wrapUnitAfter period dwell)

                            Position _ _ aheadPosition ->
                                Pixels.pixelsPerSecond 0
            }

        Timeline.TransitioningTo previous now state ->
            let
                departure =
                    getLeave lookup previous

                arrival =
                    getArrival lookup targetOccurring

                totalDuration =
                    Time.duration (Timeline.endTime previous) targetTime

                departureTime =
                    Debug.todo "Do this one"

                waitingForLateDeparture =
                    (departure.late /= 0)
                        && Time.thisBeforeThat now departureTime

                arrivalTime =
                    Debug.todo "ugh"

                arrivedEarly =
                    (arrival.early /= 0)
                        && Time.thisAfterThat now arrivalTime
            in
            -- If leaving late from `previous`,
            --    -> also start in a resting state, and then interpolate
            --
            -- if arriving early to the target state
            --    -> interpolate
            --    -> go to resting
            --
            if waitingForLateDeparture then
                let
                    departureDuration =
                        Time.duration (Timeline.endTime previous) now
                in
                move lookup targetOccurring maybeLookAhead (Timeline.Resting departureDuration state)

            else if arrivedEarly then
                let
                    arrivalDuration =
                        Time.duration arrivalTime now
                in
                move lookup targetOccurring maybeLookAhead (Timeline.Resting arrivalDuration state)

            else
                interpolateBetween lookup targetOccurring maybeLookAhead previous now state

        -- let
        --     velocityAtEndOfTransition =
        --         -- This is the velocity we're shooting for.
        --         case maybeDwell of
        --             Nothing ->
        --                 case maybeLookAhead of
        --                     Nothing ->
        --                         case lookup target of
        --                             Position _ arriving _ ->
        --                                 Pixels.pixelsPerSecond 0
        --                             Oscillate depart arriving period toX ->
        --                                 -- if there's no dwell and no lookahead,
        --                                 -- then we're approaching the last event
        --                                 -- which will "dwell" automatically
        --                                 -- until somethign happens
        --                                 derivativeOfEasing toX period 0
        --                     Just (Timeline.Occurring lookAhead aheadTime maybeLookAheadDwell) ->
        -- case lookup lookAhead of
        --     Position _ arriving aheadPosition ->
        --         velocityBetween targetPosition targetTime (Pixels.pixels aheadPosition) aheadTime
        --     Oscillate depart arriving period toX ->
        --         derivativeOfEasing toX period 0
        --             Just dwell ->
        --                 case lookup target of
        --                     Position _ arriving _ ->
        --                         Pixels.pixelsPerSecond 0
        --                     Oscillate depart arriving period toX ->
        --                         derivativeOfEasing toX period 0
        --     targetTimeInMs =
        --         Time.inMilliseconds targetTime
        --     startTimeInMs =
        --         Time.inMilliseconds (Timeline.endTime previous)
        --     curve =
        --         createSpline
        --             { start = Point2d.unitless startTimeInMs (Pixels.inPixels state.position)
        --             , startVelocity = Vector2d.unitless 1000 (Pixels.inPixelsPerSecond state.velocity)
        --             , departure = getLeave lookup previous
        --             , end = Point2d.unitless targetTimeInMs (Pixels.inPixels targetPosition)
        --             , endVelocity = Vector2d.unitless 1000 (Pixels.inPixelsPerSecond velocityAtEndOfTransition)
        --             , arrival = getArrival lookup targetOccurring
        --             }
        --     current =
        --         findAtX curve
        --             (Time.inMilliseconds currentTime)
        --             -- tolerance
        --             1
        --             -- jumpSize
        --             0.25
        --             -- starting t
        --             0.5
        --             -- depth
        --             0
        -- in
        -- { position =
        --     current.point
        --         |> Point2d.yCoordinate
        --         |> Quantity.toFloat
        --         |> Pixels.pixels
        -- , velocity =
        --     CubicSpline2d.firstDerivative curve current.t
        --         |> Vector2d.yComponent
        --         |> Quantity.toFloat
        --         |> Pixels.pixelsPerSecond
        -- }
        Timeline.Resting restingDuration state ->
            case lookup target of
                Position _ _ pos ->
                    { position = Pixels.pixels pos
                    , velocity = Pixels.pixelsPerSecond 0
                    }

                Oscillate _ _ period toX ->
                    { position = Pixels.pixels (toX (wrapUnitAfter period restingDuration))
                    , velocity = derivativeOfEasing toX period (wrapUnitAfter period restingDuration)
                    }


interpolateBetween lookup ((Timeline.Occurring target targetTime maybeDwell) as targetOccurring) maybeLookAhead previous currentTime state =
    let
        targetPosition =
            case lookup target of
                Oscillate _ _ _ toX ->
                    Pixels.pixels (toX 0)

                Position _ _ x ->
                    Pixels.pixels x

        velocityAtEndOfTransition =
            -- This is the velocity we're shooting for.
            case maybeDwell of
                Nothing ->
                    case maybeLookAhead of
                        Nothing ->
                            case lookup target of
                                Position _ arriving _ ->
                                    Pixels.pixelsPerSecond 0

                                Oscillate depart arriving period toX ->
                                    -- if there's no dwell and no lookahead,
                                    -- then we're approaching the last event
                                    -- which will "dwell" automatically
                                    -- until somethign happens
                                    derivativeOfEasing toX period 0

                        Just (Timeline.Occurring lookAhead aheadTime maybeLookAheadDwell) ->
                            case lookup lookAhead of
                                Position _ arriving aheadPosition ->
                                    velocityBetween targetPosition targetTime (Pixels.pixels aheadPosition) aheadTime

                                Oscillate depart arriving period toX ->
                                    derivativeOfEasing toX period 0

                Just dwell ->
                    case lookup target of
                        Position _ arriving _ ->
                            Pixels.pixelsPerSecond 0

                        Oscillate depart arriving period toX ->
                            derivativeOfEasing toX period 0

        targetTimeInMs =
            Time.inMilliseconds targetTime

        startTimeInMs =
            Time.inMilliseconds (Timeline.endTime previous)

        curve =
            createSpline
                { start = Point2d.unitless startTimeInMs (Pixels.inPixels state.position)
                , startVelocity = Vector2d.unitless 1000 (Pixels.inPixelsPerSecond state.velocity)
                , departure = getLeave lookup previous
                , end = Point2d.unitless targetTimeInMs (Pixels.inPixels targetPosition)
                , endVelocity = Vector2d.unitless 1000 (Pixels.inPixelsPerSecond velocityAtEndOfTransition)
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
    in
    { position =
        current.point
            |> Point2d.yCoordinate
            |> Quantity.toFloat
            |> Pixels.pixels
    , velocity =
        CubicSpline2d.firstDerivative curve current.t
            |> Vector2d.yComponent
            |> Quantity.toFloat
            |> Pixels.pixelsPerSecond
    }


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



-- findAtX : CubicSpline2d.CubicSpline2d Pixels.Pixels Pixels.Pixels -> Float -> Float -> Float -> Float -> Float -> { x : Float, y : Float }


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


color : (event -> Color.Color) -> Timeline.Occurring event -> Maybe (Timeline.Occurring event) -> Timeline.Phase event Color.Color -> Color.Color
color lookup (Timeline.Occurring target targetTime maybeDwell) maybeLookAhead phase =
    case phase of
        Timeline.Start ->
            lookup target

        Timeline.After state ->
            lookup target

        Timeline.TransitioningTo previous now state ->
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
