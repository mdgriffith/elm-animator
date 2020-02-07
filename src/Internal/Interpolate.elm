module Internal.Interpolate exposing
    ( Movement(..), startMoving, move
    , derivativeOfEasing
    , startDescription, describe
    , startLinear, linearly
    , defaultArrival, defaultDeparture
    , adjustTiming
    , startMoving2, dwellMove2, dwellPeriod, afterMove, lerp
    , coloring, createSpline, findAtXOnSpline, linearly2, moving
    )

{-|

@docs Movement, startMoving, move

@docs derivativeOfEasing

@docs startDescription, describe

@docs startLinear, linearly

@docs defaultArrival, defaultDeparture

@docs Period, adjustTiming

@docs startMoving2, dwellMove2, dwellPeriod, afterMove, lerp

-}

import Color
import Duration
import Internal.Spring as Spring
import Internal.Time as Time
import Internal.Timeline as Timeline exposing (Period(..))
import Pixels
import Quantity


{-|

    oscillate around a point

    or go to a specific position

-}
type Movement
    = Oscillate Departure Arrival Period (Float -> Float)
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


wrap : Float -> Float
wrap x =
    if x < 0 then
        1 + (x - toFloat (ceiling x))

    else
        x - toFloat (floor x)


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


startDescription : (event -> event) -> Timeline.Occurring event -> List (Timeline.Description event)
startDescription lookup (Timeline.Occurring start startTime _) =
    []


describe :
    (event -> event)
    -> Timeline.Previous event
    -> Timeline.Occurring event
    -> Maybe (Timeline.Occurring event)
    -> Timeline.Phase
    -> Time.Absolute
    -> List (Timeline.Description event)
    -> List (Timeline.Description event)
describe _ previous target _ _ _ events =
    case target of
        Timeline.Occurring targetEv targetTime _ ->
            case previous of
                Timeline.Previous _ ->
                    events ++ [ Timeline.DescribeEvent (Time.toPosix targetTime) targetEv ]

                Timeline.PreviouslyInterrupted interruptionTime ->
                    case List.reverse events of
                        [] ->
                            [ Timeline.DescribeEvent (Time.toPosix targetTime) targetEv ]

                        (Timeline.DescribeEvent interruptionTargetTime interruptedEv) :: remaining ->
                            List.reverse remaining
                                ++ [ Timeline.DescribeInterruption
                                        { interruption = Time.toPosix interruptionTime
                                        , target = interruptedEv
                                        , newTarget = targetEv
                                        , newTargetTime = Time.toPosix targetTime
                                        }
                                   ]

                        _ ->
                            events ++ [ Timeline.DescribeEvent (Time.toPosix targetTime) targetEv ]


linearly2 : Timeline.Interp event Float Float
linearly2 =
    { start = identity
    , dwellFor = \point duration -> point
    , dwellPeriod = \_ -> Nothing
    , adjustor =
        \_ ->
            { arrivingEarly = 0
            , leavingLate = 0
            }
    , after =
        \lookup target future ->
            lookup (Timeline.getEvent target)
    , lerp =
        \lookup previous ((Timeline.Occurring target targetTime maybeDwell) as targetOccurring) future now state ->
            let
                progress =
                    Time.progress
                        (Timeline.previousEndTime previous)
                        targetTime
                        now
            in
            linear state (lookup target) progress
    }


startLinear : (event -> Float) -> Timeline.Occurring event -> Float
startLinear lookup (Timeline.Occurring start startTime _) =
    lookup start


linearly : (event -> Float) -> Timeline.Previous event -> Timeline.Occurring event -> Maybe (Timeline.Occurring event) -> Timeline.Phase -> Time.Absolute -> Float -> Float
linearly lookup previous ((Timeline.Occurring target targetTime maybeDwell) as targetOccurring) maybeLookAhead phase now state =
    case phase of
        Timeline.Start ->
            lookup target

        Timeline.Transitioning ->
            let
                eventEndTime =
                    Timeline.endTime targetOccurring
            in
            if Time.thisAfterOrEqualThat now eventEndTime || Time.thisAfterOrEqualThat now targetTime then
                lookup target

            else
                let
                    progress =
                        Time.progress
                            (Timeline.previousEndTime previous)
                            targetTime
                            now
                in
                linear state (lookup target) progress


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


startMoving2 : Movement -> State
startMoving2 movement =
    { position =
        case movement of
            Oscillate _ _ _ toX ->
                Pixels.pixels (toX 0)

            Position depart arrive x ->
                Pixels.pixels x
    , velocity = Pixels.pixelsPerSecond 0
    }


adjustTiming : Movement -> Timeline.Adjustment
adjustTiming m =
    case m of
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


notInterrupted prev =
    case prev of
        Timeline.Previous _ ->
            True

        Timeline.PreviouslyInterrupted _ ->
            False


moving : Timeline.Interp event Movement State
moving =
    { start = startMoving2
    , dwellFor = dwellMove2
    , dwellPeriod = dwellPeriod
    , adjustor = adjustTiming
    , after = afterMove
    , lerp = lerp
    }


dwellPeriod : Movement -> Maybe Period
dwellPeriod movement =
    case movement of
        Position _ _ _ ->
            Nothing

        Oscillate _ _ period _ ->
            Just period


dwellMove2 : Movement -> Time.Duration -> State
dwellMove2 movement duration =
    case movement of
        Position _ _ pos ->
            { position = Pixels.pixels pos
            , velocity = Pixels.pixelsPerSecond 0
            }

        Oscillate _ _ period toX ->
            case period of
                Loop periodDuration ->
                    let
                        progress =
                            wrapUnitAfter periodDuration duration
                    in
                    { position = Pixels.pixels (toX progress)
                    , velocity = derivativeOfEasing toX periodDuration progress
                    }

                Repeat n periodDuration ->
                    let
                        iterationTimeMS =
                            Duration.inMilliseconds periodDuration

                        totalMS =
                            Duration.inMilliseconds duration

                        iteration =
                            floor (totalMS / iterationTimeMS)
                    in
                    if iteration >= n then
                        { position = Pixels.pixels (toX 1)
                        , velocity = Pixels.pixelsPerSecond 0
                        }

                    else
                        let
                            progress =
                                wrapUnitAfter periodDuration duration
                        in
                        { position = Pixels.pixels (toX progress)
                        , velocity = derivativeOfEasing toX periodDuration progress
                        }


lerp lookup previous target future now state =
    let
        wobble =
            case lookup (Timeline.getEvent target) of
                Oscillate _ arrival _ _ ->
                    arrival.wobbliness

                Position _ arrival _ ->
                    arrival.wobbliness
    in
    if future == [] && wobble /= 0 then
        springInterpolation lookup previous target now state

    else
        interpolateBetween lookup previous target (List.head future) now state


afterMove lookup target future =
    { position =
        case lookup (Timeline.getEvent target) of
            Oscillate depart arrive period toX ->
                Pixels.pixels (toX 0)

            Position _ _ x ->
                Pixels.pixels x
    , velocity =
        velocityAtTarget lookup target (List.head future)
    }


{-| -}
move : (event -> Movement) -> Timeline.Previous event -> Timeline.Occurring event -> Maybe (Timeline.Occurring event) -> Timeline.Phase -> Time.Absolute -> State -> State
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
            if log "dwell-prev" <| Time.thisBeforeOrEqualThat now (Timeline.previousEndTime previous) && notInterrupted previous then
                -- we're dwelling at prev
                -- dwell till `now`
                let
                    previousOccurringEvent =
                        case previous of
                            Timeline.Previous p ->
                                Timeline.getEvent p

                            Timeline.PreviouslyInterrupted _ ->
                                -- This is incorrect, but this branch is
                                -- protected by the above if statement
                                -- sorta awkward
                                Timeline.getEvent target

                    previousStartTime =
                        case previous of
                            Timeline.Previous p ->
                                Timeline.startTime p

                            Timeline.PreviouslyInterrupted _ ->
                                -- This is incorrect, but this branch is
                                -- protected by the above if statement
                                -- sorta awkward
                                Timeline.startTime target

                    endTime =
                        Time.earliest
                            (Timeline.previousEndTime previous)
                            now
                in
                dwellFor (lookup previousOccurringEvent)
                    (Time.duration previousStartTime endTime)

            else if log "lerp" <| Time.thisBeforeOrEqualThat now (Timeline.startTime target) then
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
            case period of
                Loop periodDuration ->
                    let
                        progress =
                            wrapUnitAfter periodDuration duration
                    in
                    { position = Pixels.pixels (toX progress)
                    , velocity = derivativeOfEasing toX periodDuration progress
                    }

                Repeat n periodDuration ->
                    let
                        iterationTimeMS =
                            Duration.inMilliseconds periodDuration

                        totalMS =
                            Duration.inMilliseconds duration

                        iteration =
                            floor (totalMS / iterationTimeMS)
                    in
                    if iteration >= n then
                        { position = Pixels.pixels (toX 1)
                        , velocity = Pixels.pixelsPerSecond 0
                        }

                    else
                        let
                            progress =
                                wrapUnitAfter periodDuration duration
                        in
                        { position = Pixels.pixels (toX progress)
                        , velocity = derivativeOfEasing toX periodDuration progress
                        }


{-| -}
springInterpolation : (event -> Movement) -> Timeline.Previous event -> Timeline.Occurring event -> Time.Absolute -> State -> State
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
            Time.duration (Timeline.previousEndTime previous) (Timeline.startTime target)

        params =
            Spring.select wobble duration

        new =
            Spring.stepOver
                (Time.duration (Timeline.previousEndTime previous) now)
                params
                targetPos
                { position = Pixels.inPixels state.position
                , velocity = Pixels.inPixelsPerSecond state.velocity
                }
    in
    { position = Pixels.pixels new.position
    , velocity = Pixels.pixelsPerSecond new.velocity
    }


interpolateBetween : (event -> Movement) -> Timeline.Previous event -> Timeline.Occurring event -> Maybe (Timeline.Occurring event) -> Time.Absolute -> State -> State
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
            Time.inMilliseconds (Timeline.previousEndTime previous)

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
                , departure =
                    case previous of
                        Timeline.Previous prevOccurring ->
                            getLeave lookup prevOccurring

                        Timeline.PreviouslyInterrupted _ ->
                            nullDeparture
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

        -- *NOTE* We'll probably want to do oscillation mixing in the future, but it's kinda involved.
        -- This would get you some of the way there (where maybeMixing has a postion/velocity to use instead)
        -- but it doesn't quite work to mix two oscillators in a direct way because of weird descructive interference.
        -- and looks especially weird when the effective origin of the oscillation actually moves.
        -- HOWEVER, I'm open to creative solutions :D
        -- maybeMix =
        --     case lookup target of
        --         Oscillate _ mixArrival mixBPeriod mixB ->
        --             case previous of
        --                 Timeline.Previous p ->
        --                     case lookup (Timeline.getEvent p) of
        --                         Position _ _ _ ->
        --                             Nothing
        --                         Oscillate mixDeparture _ mixAPeriod mixA ->
        --                             if Timeline.hasDwell p && (maybeLookAhead == Nothing || Timeline.hasDwell targetOccurring) then
        --                                 let
        --                                     {-
        --                                        mixA starts at
        --                                            (previousStartTime previous) ->
        --                                                    roll mixA forward to previousEndTime
        --                                        mixB starts at
        --                                            (previousEndTime previous)
        --                                            but must match 0 for 0 at targetTime
        --                                            rephase mixB
        --                                            progress is the percentage progress between
        --                                                previousStartTime + previousEndTime
        --                                     -}
        --                                     phaseBShift =
        --                                         wrapUnitAfter mixBPeriod totalDuration
        --                                     phaseAShift =
        --                                         wrapUnitAfter mixAPeriod
        --                                             (Time.duration
        --                                                 (Timeline.previousEndTime previous)
        --                                                 (Timeline.previousStartTime previous)
        --                                             )
        --                                     totalDuration =
        --                                         Time.duration (Timeline.previousEndTime previous) targetTime
        --                                     progress =
        --                                         Time.progress (Timeline.previousEndTime previous) targetTime currentTime
        --                                     correctedA u =
        --                                         (wrapUnitAfter mixAPeriod (Quantity.multiplyBy u totalDuration)
        --                                             + phaseAShift
        --                                         )
        --                                             |> wrap
        --                                             |> mixA
        --                                     correctedB u =
        --                                         (wrapUnitAfter mixBPeriod (Quantity.multiplyBy u totalDuration)
        --                                             - phaseBShift
        --                                         )
        --                                             |> wrap
        --                                             |> mixB
        --                                     newEasing masterU =
        --                                         mix correctedA correctedB progress masterU
        --                                 in
        --                                 Just
        --                                     { position = Pixels.pixels (newEasing progress)
        --                                     , velocity = derivativeOfEasing newEasing totalDuration progress
        --                                     }
        --                             else
        --                                 Nothing
        --                 Timeline.PreviouslyInterrupted _ ->
        --                     Nothing
        --         Position _ _ x ->
        --             Nothing
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
                            case period of
                                Loop dur ->
                                    derivativeOfEasing toX dur 0

                                Repeat n dur ->
                                    if n == 0 then
                                        Pixels.pixelsPerSecond 0

                                    else
                                        derivativeOfEasing toX dur 0

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
                                    case period of
                                        Loop periodDuration ->
                                            derivativeOfEasing toX periodDuration 0

                                        Repeat n periodDuration ->
                                            derivativeOfEasing toX periodDuration 0

        Just dwell ->
            case lookup target of
                Position _ _ _ ->
                    Pixels.pixelsPerSecond 0

                Oscillate _ _ period toX ->
                    case period of
                        Loop periodDuration ->
                            derivativeOfEasing toX periodDuration 0

                        Repeat n periodDuration ->
                            derivativeOfEasing toX periodDuration 0


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

            else if
                ((config.startVelocity.x - zeroPoint.x) == 0)
                    && ((config.startVelocity.y - zeroPoint.y) == 0)
            then
                -- scaleBy (config.departure.slowly * 3)
                { x = totalX * (config.departure.slowly * 3)
                , y = 0
                }

            else
                -- config.startVelocity
                --     |> scaleBy (config.departure.slowly * 3)
                { x = config.startVelocity.x * (config.departure.slowly * 3)
                , y = config.startVelocity.y * (config.departure.slowly * 3)
                }

        endVelocity =
            if config.arrival.slowly == 0 then
                config.endVelocity

            else if
                ((config.endVelocity.x - zeroPoint.x) == 0)
                    && ((config.endVelocity.y - zeroPoint.y) == 0)
            then
                -- scaleBy
                { x = totalX * (config.arrival.slowly * 3)
                , y = 0
                }

            else
                -- config.endVelocity
                --     |> scaleBy (config.arrival.slowly * 3)
                { x = config.endVelocity.x * (config.arrival.slowly * 3)
                , y = config.endVelocity.y * (config.arrival.slowly * 3)
                }
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
        { x = config.start.x + ((1 / 3) * startVelocity.x)
        , y = config.start.y + ((1 / 3) * startVelocity.y)
        }
        { x = config.end.x + ((-1 / 3) * endVelocity.x)
        , y = config.end.y + ((-1 / 3) * endVelocity.y)
        }
        -- (config.start |> translateBy (scaleBy (1 / 3) startVelocity))
        -- (config.end |> translateBy (scaleBy (-1 / 3) endVelocity))
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
        interp from to v =
            from + (to - from) * v

        pair interpolate ( a0, b0 ) ( a1, b1 ) v =
            ( interpolate a0 a1 v, interpolate b0 b1 v )

        casteljau ps =
            case ps of
                [ ( x, y ) ] ->
                    y

                xs ->
                    List.map2 (\x y -> pair interp x y time) xs (Maybe.withDefault [] (List.tail xs))
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
            two
                |> Quantity.minus one

        duration =
            Time.duration oneTime twoTime

        vel =
            distance |> Quantity.per duration
    in
    if Quantity.isNaN vel || Quantity.isInfinite vel then
        Quantity.zero

    else
        vel


coloring : Timeline.Interp event Color.Color Color.Color
coloring =
    { start = identity
    , dwellFor = \clr duration -> clr
    , dwellPeriod = \_ -> Nothing
    , adjustor =
        \_ ->
            { arrivingEarly = 0
            , leavingLate = 0
            }
    , after =
        \lookup target future ->
            lookup (Timeline.getEvent target)
    , lerp = lerpColor
    }


lerpColor lookup previous ((Timeline.Occurring target targetTime maybeDwell) as targetOccurring) future now state =
    let
        progress =
            Time.progress
                (Timeline.previousEndTime previous)
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


{-| Mix two easing functions.

    - weightB is how much of `b` should be present.
    - percent is where we are between 0 and 1

So,
weightB of 0 means we're just using easing a.
weightB of 1 means we're just using easing b.
weightB of 0.5 means we're taking the value at a and value at b and weighting them both by 0.5.

-}
mix : (Float -> Float) -> (Float -> Float) -> Float -> Float -> Float
mix fnA fnB weightB percent =
    let
        a =
            fnA percent

        b =
            fnB percent
    in
    -- a + weightB * (b - a)
    ((1 - weightB) * a) + (weightB * b)



{- A mini embedded elm-geometry because I didn't want to impose it as a dependency.

   However!  It's definitely a package worth checking out!

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
pointOn ((Spline p1 p2 p3 p4) as s) proportion =
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
findAtXOnSpline ((Spline p1 p2 p3 p4) as spline) desiredX tolerance jumpSize t depth =
    let
        point =
            if t <= 0.5 then
                let
                    q1 =
                        { x = p1.x + t * (p2.x - p1.x)
                        , y = p1.y + t * (p2.y - p1.y)
                        }

                    q2 =
                        { x = p2.x + t * (p3.x - p2.x)
                        , y = p2.y + t * (p3.y - p2.y)
                        }

                    q3 =
                        { x = p3.x + t * (p4.x - p3.x)
                        , y = p3.y + t * (p4.y - p3.y)
                        }

                    r1 =
                        { x = q1.x + t * (q2.x - q1.x)
                        , y = q1.y + t * (q2.y - q1.y)
                        }

                    r2 =
                        { x = q2.x + t * (q3.x - q2.x)
                        , y = q2.y + t * (q3.y - q2.y)
                        }
                in
                { x = r1.x + t * (r2.x - r1.x)
                , y = r1.y + t * (r2.y - r1.y)
                }

            else
                let
                    q1 =
                        { x = p2.x + (1 - t) * (p1.x - p2.x)
                        , y = p2.y + (1 - t) * (p1.y - p2.y)
                        }

                    q2 =
                        { x = p3.x + (1 - t) * (p2.x - p3.x)
                        , y = p3.y + (1 - t) * (p2.y - p3.y)
                        }

                    q3 =
                        { x = p4.x + (1 - t) * (p3.x - p4.x)
                        , y = p4.y + (1 - t) * (p3.y - p4.y)
                        }

                    r1 =
                        { x = q2.x + (1 - t) * (q1.x - q2.x)
                        , y = q2.y + (1 - t) * (q1.y - q2.y)
                        }

                    r2 =
                        { x = q3.x + (1 - t) * (q2.x - q3.x)
                        , y = q3.y + (1 - t) * (q2.y - q3.y)
                        }
                in
                { x = r2.x + (1 - t) * (r1.x - r2.x)
                , y = r2.y + (1 - t) * (r1.y - r2.y)
                }
    in
    if depth == 10 then
        { point = point
        , t = t
        }

    else if abs (point.x - desiredX) < 1 && abs (point.x - desiredX) >= 0 then
        { point = point
        , t = t
        }

    else if (point.x - desiredX) > 0 then
        findAtXOnSpline spline desiredX tolerance (jumpSize / 2) (t - jumpSize) (depth + 1)

    else
        findAtXOnSpline spline desiredX tolerance (jumpSize / 2) (t + jumpSize) (depth + 1)
