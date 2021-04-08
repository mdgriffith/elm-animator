module Internal.Interpolate exposing
    ( Movement(..), State, derivativeOfEasing
    , dwellPeriod
    , coloring, linearly, moving, mapPersonality
    , Checkpoint, Oscillator(..), Personality, Point, Sequence(..), Step(..), Timing(..), base, color, createSpline, details, equalState, lerpSplines, linearDefault, standardDefault, visit
    )

{-|

@docs Movement, State, derivativeOfEasing

@docs Period

@docs startMoving, dwellPeriod

@docs coloring, linearly, moving, mapPersonality

-}

import Color
import Duration
import Html.Events exposing (preventDefaultOn)
import Internal.Bezier as Bezier
import Internal.Spring as Spring
import Internal.Time as Time
import Internal.Timeline as Timeline exposing (Period(..))
import Internal.Transition as Transition
import Pixels
import Quantity


{-| -}
type Movement
    = Pos Personality Float (Maybe (Sequence Float))


{-| -}
type Sequence value
    = Repeat Int (List (Step value))


type Step value
    = Step Duration.Duration Transition.Transition value
    | Wait Duration.Duration


{-| @deprecated
-}
type alias Checkpoint =
    { value : Float

    -- timing function to get to the above value
    , timing : Timing

    -- percentage time when we've arrived at `value`
    , time : Float
    }


type Timing
    = Linear
    | Bezier Bezier.Spline


{-| -}
type Oscillator
    = Oscillator Float (List Checkpoint)
    | Resting Float


type alias Personality =
    { wobbliness : Proportion
    , impulse : Proportion
    , arriveEarly : Proportion
    , arriveSlowly : Proportion
    , departLate : Proportion
    , departSlowly : Proportion
    }


{-| Number betwen 0 and 1
-}
type alias Proportion =
    Float


standardDefault : Personality
standardDefault =
    { departLate = 0
    , departSlowly = 0.4
    , wobbliness = 0
    , impulse = 0
    , arriveEarly = 0
    , arriveSlowly = 0.8
    }


linearDefault : Personality
linearDefault =
    { departLate = 0
    , departSlowly = 0
    , wobbliness = 0
    , impulse = 0
    , arriveEarly = 0
    , arriveSlowly = 0
    }


base : Movement -> Float
base (Pos _ f _) =
    f


mapPersonality fn movement =
    case movement of
        Pos p f seq ->
            Pos (fn p) f seq


wrap : Float -> Float
wrap x =
    if x < 0 then
        1 + (x - toFloat (ceiling x))

    else
        x - toFloat (floor x)


wrapUnitAfter : Duration.Duration -> Duration.Duration -> Float
wrapUnitAfter dur total =
    let
        periodDuration =
            round (Duration.inMilliseconds dur)

        totalDuration =
            round (Duration.inMilliseconds total)
    in
    if periodDuration == 0 || totalDuration == 0 then
        0

    else
        let
            remaining =
                totalDuration |> modBy periodDuration
        in
        if remaining == 0 then
            1

        else
            toFloat remaining
                / toFloat periodDuration


type alias Progress =
    Float


type alias State =
    { position : Pixels
    , velocity : PixelsPerSecond
    }


equalState : State -> State -> Bool
equalState one two =
    (Pixels.inPixels one.position - Pixels.inPixels two.position == 0)
        && (Pixels.inPixelsPerSecond one.velocity - Pixels.inPixelsPerSecond two.velocity == 0)


type alias XY thing =
    { x : thing
    , y : thing
    }


linearly : Timeline.Interp event Float Float
linearly =
    { start = identity
    , dwellPeriod = \_ -> Nothing
    , adjustor =
        \_ ->
            linearDefault
    , visit =
        \lookup target now maybeLookAhead state ->
            lookup (Timeline.getEvent target)
    , lerp =
        \prevEndTime maybePrev target targetTime now maybeLookAhead state ->
            -- target
            -- Transitioning 0
            let
                progress =
                    Time.progress
                        prevEndTime
                        targetTime
                        now
            in
            linear state target progress
    }


startMoving : Movement -> State
startMoving movement =
    { position =
        case movement of
            Pos _ x _ ->
                Pixels.pixels x
    , velocity = Pixels.pixelsPerSecond 0
    }


getPersonality : Movement -> Personality
getPersonality (Pos personality _ _) =
    personality


zeroDuration : Duration.Duration
zeroDuration =
    Duration.milliseconds 0


{-| -}
details : Timeline.Timeline state -> (state -> Movement) -> { position : Float, velocity : Float }
details timeline lookup =
    unwrapUnits
        (Timeline.foldp
            lookup
            moving
            timeline
        )


unwrapUnits { position, velocity } =
    { position =
        case position of
            Quantity.Quantity val ->
                val
    , velocity =
        case velocity of
            Quantity.Quantity val ->
                val
    }


moving : Timeline.Interp state Movement State
moving =
    { start = startMoving
    , dwellPeriod = dwellPeriod
    , adjustor = getPersonality
    , visit = visit
    , lerp = lerp
    }


dwellPeriod : Movement -> Maybe Period
dwellPeriod movement =
    -- THIS IS GOING AWAY IM PRETTY SURE
    Nothing


{-| This is the combination of the previous `dwellFor` and `after` functions.

Given an `Occurring`.

We want to dwell at this event until a certain time.

That time could be:

  - Till the end of this event
  - Till `now` (we have an end recorded, but we're not there yet)

If there is nothing to lookahead to, we want to dwell till `now`, even if it's passed the event end.

If we're not dwelling at all (i.e. dwell time is 0),
then we can estimate what our velocity would be by looking ino the future.

-}
visit :
    (event -> Movement)
    -> Timeline.Occurring event
    -> Time.Absolute
    -> Maybe (Timeline.LookAhead Movement)
    -> State
    -> State
visit lookup ((Timeline.Occurring event start eventEnd) as occurring) now maybeLookAhead state =
    let
        -- _ =
        --     Debug.log "    VISIT"
        --         { event = event
        --         , state = state
        --         , now = now
        --         , lookAhead = maybeLookAhead
        --         }
        dwellTime =
            case maybeLookAhead of
                Nothing ->
                    Time.duration start now

                _ ->
                    Time.duration start (Time.earliest now eventEnd)

        -- _ =
        --     Debug.log "DWELL TIME" ( now, start, eventEnd )
    in
    -- Debug.log "   <-" <|
    if Time.zeroDuration dwellTime then
        { position =
            case lookup event of
                Pos _ x _ ->
                    Pixels.pixels x
        , velocity =
            newVelocityAtTarget (lookup event)
                start
                maybeLookAhead
        }

    else
        case lookup event of
            Pos _ pos Nothing ->
                { position = Pixels.pixels pos
                , velocity = Pixels.pixelsPerSecond 0
                }

            Pos _ pos (Just seq) ->
                -- oscillate target period points dwellTime
                Debug.todo ""


type alias Milliseconds =
    Float


lerpSplines : Time.Absolute -> Movement -> Movement -> Time.Absolute -> Maybe (Timeline.LookAhead Movement) -> State -> List Bezier.Spline
lerpSplines prevEndTime prev target targetTime maybeLookAhead state =
    let
        wobble =
            case target of
                Pos personality _ _ ->
                    personality.wobbliness

        shouldWobble =
            case maybeLookAhead of
                Nothing ->
                    wobble /= 0

                _ ->
                    False

        targetPos =
            case target of
                Pos _ x _ ->
                    x
    in
    if shouldWobble then
        let
            params =
                Spring.select wobble
                    (Time.duration
                        prevEndTime
                        targetTime
                    )
        in
        Spring.segments params
            { position = Pixels.inPixels state.position
            , velocity = Pixels.inPixelsPerSecond state.velocity
            }
            targetPos

    else
        let
            targetVelocity =
                newVelocityAtTarget target targetTime maybeLookAhead
                    |> Pixels.inPixelsPerSecond
        in
        [ createSpline
            { start =
                { x = Time.inMilliseconds prevEndTime
                , y = Pixels.inPixels state.position
                }
            , startVelocity =
                { x = 1000
                , y = Pixels.inPixelsPerSecond state.velocity
                }
            , departure =
                case prev of
                    Pos personality _ _ ->
                        personality
            , end =
                { x = Time.inMilliseconds targetTime
                , y = targetPos
                }
            , endVelocity =
                { x = 1000
                , y = targetVelocity
                }
            , arrival =
                case target of
                    Pos personality _ _ ->
                        personality
            }
        ]


lerp : Time.Absolute -> Movement -> Movement -> Time.Absolute -> Time.Absolute -> Maybe (Timeline.LookAhead Movement) -> State -> State
lerp prevEndTime prev target targetTime now maybeLookAhead state =
    let
        -- _ =
        -- Debug.log "    LERP"
        --     { prevEndTime = prevEndTime
        --     , prev = prev
        --     , target = target
        --     , targetTime = targetTime
        --     , now = now
        --     , maybeLookAhead = maybeLookAhead
        --     , state = state
        --     }
        wobble =
            case target of
                Pos personality _ _ ->
                    personality.wobbliness

        nothingHappened =
            case target of
                Pos _ x Nothing ->
                    (x == Pixels.inPixels state.position)
                        && (Pixels.inPixelsPerSecond state.velocity == 0)

                Pos _ _ (Just _) ->
                    False
    in
    -- Debug.log "   <-" <|
    if nothingHappened then
        state

    else
        case maybeLookAhead of
            Nothing ->
                if wobble /= 0 then
                    springInterpolation prevEndTime prev target targetTime now maybeLookAhead state

                else
                    interpolateBetween prevEndTime prev target targetTime now maybeLookAhead state

            _ ->
                interpolateBetween prevEndTime prev target targetTime now maybeLookAhead state


{-| -}
springInterpolation :
    Time.Absolute
    -> Movement
    -> Movement
    -> Time.Absolute
    -> Time.Absolute
    -> Maybe (Timeline.LookAhead Movement)
    -> State
    -> State
springInterpolation prevEndTime _ target targetTime now _ state =
    let
        wobble =
            case target of
                Pos personality _ _ ->
                    personality.wobbliness

        targetPos =
            case target of
                Pos _ x _ ->
                    x

        duration =
            Time.duration prevEndTime targetTime

        params =
            Spring.select wobble duration

        new =
            Spring.analytical params
                (Time.duration prevEndTime now)
                targetPos
                { position = Pixels.inPixels state.position
                , velocity = Pixels.inPixelsPerSecond state.velocity
                }
    in
    { position = Pixels.pixels new.position
    , velocity = Pixels.pixelsPerSecond new.velocity
    }


interpolateBetween : Time.Absolute -> Movement -> Movement -> Time.Absolute -> Time.Absolute -> Maybe (Timeline.LookAhead Movement) -> State -> State
interpolateBetween startTimeInMs previous target targetTimeInMs now maybeLookAhead state =
    let
        targetPosition =
            case target of
                Pos _ x _ ->
                    Pixels.pixels x

        targetVelocity =
            Pixels.inPixelsPerSecond
                (newVelocityAtTarget target targetTimeInMs maybeLookAhead)

        curve =
            createSpline
                { start =
                    { x = Time.inMilliseconds startTimeInMs
                    , y = Pixels.inPixels state.position
                    }
                , startVelocity =
                    { x = 1000
                    , y = Pixels.inPixelsPerSecond state.velocity
                    }
                , departure =
                    case previous of
                        Pos personality _ _ ->
                            personality
                , end =
                    { x = Time.inMilliseconds targetTimeInMs
                    , y = Pixels.inPixels targetPosition
                    }
                , endVelocity =
                    { x = 1000
                    , y = targetVelocity
                    }
                , arrival =
                    case target of
                        Pos personality _ _ ->
                            personality
                }

        current =
            Bezier.atX (Time.inMilliseconds now) curve

        firstDerivative =
            Bezier.firstDerivative curve current.t

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



{- SEQUQENCER


   The sequence can be of:
       Float
       Color
       Frame

   We generally want a velocity and a value out of this.

        - velocity -> predict how fast we should be going as we arrive into this state.
        - position -> where are we

    For Color and Frame, we care only about `progress` between two values

-}


zeroVelocity : PixelsPerSecond
zeroVelocity =
    Pixels.pixelsPerSecond 0


initialSequenceVelocity : Sequence value -> PixelsPerSecond
initialSequenceVelocity seq =
    case seq of
        Repeat 0 _ ->
            zeroVelocity

        Repeat _ [] ->
            zeroVelocity

        Repeat n ((Wait _) :: _) ->
            zeroVelocity

        Repeat n ((Step dur transition _) :: _) ->
            Transition.initialVelocity transition
                |> (*) 1000
                |> Pixels.pixelsPerSecond


{-| -}
sequence :
    Float
    -> Sequence Float
    -> Duration.Duration
    -> State
sequence start seq duration =
    Debug.todo ""


{-| -}
sequenceProgress :
    value
    -> Sequence value
    -> Duration.Duration
    ->
        { progress : Float
        , before : value
        , after : value
        }
sequenceProgress start seq duration =
    Debug.todo ""



{- OSCILLATOR INTERPOLATION

   THIS NEEDS TO BE REWRITTEN TO USE SEQUENCE


       oscillate : Float -> Period -> List Checkpoint -> Duration.Duration -> State
       oscillate start period points dwellTime =
           case points of
               [] ->
                   { position = Pixels.pixels start
                   , velocity = Pixels.pixelsPerSecond 0
                   }

               _ ->
                   let
                       totalPeriodDuration =
                           case period of
                               Loop periodDuration ->
                                   periodDuration

                               Repeat _ periodDuration ->
                                   periodDuration

                       -- _ = Debug.log "PERIODS"
                       --     { dur = totalPeriodDuration
                       --     , dwellTime = dwellTime
                       --     , periodPercentage = percentage
                       --     }
                       percentage =
                           case period of
                               Loop periodDuration ->
                                   wrapUnitAfter periodDuration dwellTime

                               Repeat n periodDuration ->
                                   let
                                       iterationTimeMS =
                                           Duration.inMilliseconds periodDuration

                                       totalMS =
                                           Duration.inMilliseconds dwellTime

                                       iteration =
                                           floor (totalMS / iterationTimeMS)
                                   in
                                   if iteration >= n then
                                       1

                                   else
                                       wrapUnitAfter periodDuration dwellTime
                   in
                   oscillateHelper totalPeriodDuration start points percentage 0 dwellTime


       oscillateHelper periodDuration previous points currentTime previousTime dwellTime =
           case points of
               [] ->
                   { position = Pixels.pixels previous
                   , velocity = Pixels.pixelsPerSecond 0
                   }

               checkpoint :: remaining ->
                   if currentTime > checkpoint.time then
                       oscillateHelper periodDuration checkpoint.value remaining currentTime checkpoint.time dwellTime

                   else
                       let
                           -- _ = Debug.log "OSCILLATION" (progress, end)
                           -- _ = Debug.log "times"
                           --     { currentTime = currentTime
                           --     , previousTime = previousTime
                           --     , checkpoint = checkpoint.time
                           --     , subPeriod = subPeriodDuration
                           --     , progress = progress
                           --     }
                           progress =
                               (currentTime - previousTime)
                                   / (checkpoint.time - previousTime)

                           end =
                               case remaining of
                                   [] ->
                                       1

                                   next :: _ ->
                                       next.time

                           -- This is the duration of the current section we're on
                           -- which is only a piece of the total repeating pattern
                           subPeriodDuration =
                               Quantity.multiplyBy (end - checkpoint.time) periodDuration
                       in
                       atTiming subPeriodDuration checkpoint.timing dwellTime previous checkpoint.value


       atTiming subPeriodDuration timing time start target =
           case timing of
               Linear ->
                   let
                       -- BUGBUG this needs to be calculated from time
                       percent =
                           0.5
                   in
                   { position = Pixels.pixels (linear start target percent)
                   , velocity =
                       Pixels.pixelsPerSecond
                           ((target - start)
                               / Duration.inSeconds subPeriodDuration
                           )
                   }

               Bezier spline ->
                   let
                       -- totalMs = Duration.inMilliseconds subPeriodDuration
                       -- time = totalMs * percent
                       -- percent = 25
                       -- _ = Debug.log "AT TIMEING"
                       --     { period = subPeriodDuration
                       --     -- , percent = percent
                       --     , timing = spline
                       --     , start = start
                       --     , target = target
                       --     , current = current
                       --     , time = time
                       --     }
                       current =
                           Bezier.atX (Duration.inMilliseconds time) spline

                       firstDerivative =
                           let
                               t =
                                   -- at t == 0, the first derivative vector will always be 0,0
                                   -- so we cheat in slightly.
                                   if Duration.inMilliseconds time == 0 then
                                       0.001

                                   else
                                       current.t
                           in
                           Bezier.firstDerivative spline t
                   in
                   { position =
                       Pixels.pixels
                           current.point.y
                   , velocity =
                       -- rescale velocity so that it's pixels/second
                       -- `createSpline` scales this vector sometimes, we need to ensure it's the right size.
                       if firstDerivative.x == 0 then
                           Pixels.pixelsPerSecond 0

                       else
                           (firstDerivative.y / firstDerivative.x)
                               |> (*) 1000
                               |> Pixels.pixelsPerSecond

                   --  Pixels.pixelsPerSecond 0
                   }



-}


{-| -}
newVelocityAtTarget :
    Movement
    -> Time.Absolute
    -> Maybe (Timeline.LookAhead Movement)
    -> PixelsPerSecond
newVelocityAtTarget target targetTime maybeLookAhead =
    case maybeLookAhead of
        Nothing ->
            case target of
                Pos _ _ Nothing ->
                    zeroVelocity

                Pos _ _ (Just seq) ->
                    initialSequenceVelocity seq

        Just lookAhead ->
            let
                targetPosition =
                    case target of
                        Pos _ x _ ->
                            Pixels.pixels x
            in
            case lookAhead.anchor of
                Pos _ aheadPosition Nothing ->
                    -- our target velocity is the linear velocity between target and lookahead
                    velocityBetween
                        targetPosition
                        targetTime
                        (Pixels.pixels aheadPosition)
                        lookAhead.time

                Pos _ aheadPosition (Just seq) ->
                    if lookAhead.resting then
                        initialSequenceVelocity seq

                    else
                        velocityBetween
                            targetPosition
                            targetTime
                            (Pixels.pixels aheadPosition)
                            lookAhead.time


zeroPoint : Point
zeroPoint =
    { x = 0
    , y = 0
    }


type alias Point =
    { x : Float
    , y : Float
    }


scaleAbout : Point -> Float -> Point -> Point
scaleAbout p0 k p =
    { x = p0.x + k * (p.x - p0.x)
    , y = p0.y + k * (p.y - p0.y)
    }


scaleTo : Float -> Point -> Point
scaleTo q v =
    let
        largestComponent =
            max (abs v.x) (abs v.y)
    in
    if largestComponent == 0 then
        zeroPoint

    else
        let
            scaledX =
                v.x / largestComponent

            scaledY =
                v.y / largestComponent

            scaledLength =
                sqrt (scaledX * scaledX + scaledY * scaledY)
        in
        { x = q * scaledX / scaledLength
        , y = q * scaledY / scaledLength
        }


scaleBy : Float -> Point -> Point
scaleBy k v =
    { x = k * v.x
    , y = k * v.y
    }


distanceFrom p1 p2 =
    let
        deltaX =
            p2.x - p1.x

        deltaY =
            p2.y - p1.y

        largestComponent =
            max (abs deltaX) (abs deltaY)
    in
    if largestComponent == 0 then
        0

    else
        let
            scaledX =
                deltaX / largestComponent

            scaledY =
                deltaY / largestComponent

            scaledLength =
                sqrt (scaledX * scaledX + scaledY * scaledY)
        in
        scaledLength * largestComponent


{-| We are vector v and want the component in the direction d.
-}
componentIn : Point -> Point -> Float
componentIn d v =
    v.x * d.x + v.y * d.y


createSpline :
    { start : Point
    , startVelocity : Point
    , departure : Personality
    , end : Point
    , endVelocity : Point
    , arrival : Personality
    }
    -> Bezier.Spline
createSpline config =
    let
        totalX =
            config.end.x - config.start.x

        startVelScale =
            1 / (config.startVelocity.x / totalX)

        endVelScale =
            1 / (config.endVelocity.x / totalX)

        startVelocity =
            if config.departure.departSlowly == 0 then
                -- this is linear,
                -- we don't care how fast we were going previously.
                { x = 0
                , y = 0
                }

            else if
                ((config.startVelocity.x - zeroPoint.x) == 0)
                    && ((config.startVelocity.y - zeroPoint.y) == 0)
            then
                -- scaleBy (config.departure.slowly * 3)
                { x = totalX * (config.departure.departSlowly * 3)
                , y = 0
                }

            else
                let
                    direction =
                        { x =
                            startVelScale
                                * config.startVelocity.x
                        , y =
                            startVelScale
                                * config.startVelocity.y
                        }

                    directedDistance =
                        componentIn (scaleTo 1 direction)
                            { x = config.end.x - config.start.x
                            , y = config.end.y - config.start.y
                            }
                in
                direction
                    |> scaleTo (config.departure.departSlowly * directedDistance * 3)

        endVelocity =
            if config.arrival.arriveSlowly == 0 then
                -- if this is 0, this is linear,
                { x = 0
                , y = 0
                }

            else if
                ((config.endVelocity.x - zeroPoint.x) == 0)
                    && ((config.endVelocity.y - zeroPoint.y) == 0)
            then
                -- scaleBy
                { x = totalX * (config.arrival.arriveSlowly * 3)
                , y = 0
                }

            else
                let
                    direction =
                        { x =
                            endVelScale
                                * config.endVelocity.x
                        , y =
                            endVelScale
                                * config.endVelocity.y
                        }

                    directedDistance =
                        componentIn (scaleTo 1 direction)
                            { x = config.end.x - config.start.x
                            , y = config.end.y - config.start.y
                            }
                in
                direction
                    |> scaleTo (config.departure.arriveSlowly * directedDistance * 3)

        maxX =
            config.end.x - config.start.x

        startControl =
            { x = config.start.x + ((1 / 3) * startVelocity.x)
            , y = config.start.y + ((1 / 3) * startVelocity.y)
            }

        endControl =
            { x = config.end.x + ((-1 / 3) * endVelocity.x)
            , y = config.end.y + ((-1 / 3) * endVelocity.y)
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
    Bezier.Spline
        config.start
        (if config.end.x - startControl.x > maxX then
            startControl
                |> scaleAbout config.start (1 / ((config.start.x - startControl.x) / maxX))

         else
            startControl
        )
        (if config.end.x - endControl.x > maxX then
            endControl
                |> scaleAbout config.end (1 / ((config.end.x - endControl.x) / maxX))

         else
            endControl
        )
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
    , dwellPeriod = \_ -> Nothing
    , adjustor =
        \_ ->
            linearDefault
    , visit =
        \lookup target now maybeLookAhead state ->
            lookup (Timeline.getEvent target)
    , lerp = lerpColor
    }


lerpColor prevEndTime maybePrev target targetTime now maybeLookAhead state =
    let
        progress =
            Time.progress
                prevEndTime
                targetTime
                now
    in
    color progress state target


color : Float -> Color.Color -> Color.Color -> Color.Color
color progress from to =
    let
        one =
            Color.toRgba from

        two =
            Color.toRgba to
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
