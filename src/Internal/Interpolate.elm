module Internal.Interpolate exposing
    ( Movement, State, derivativeOfEasing
    , coloring, moving
    , Checkpoint, Oscillator(..), Point, Sequence(..), Step(..), Timing(..), color, details, equalState, getIterationCssString, getPeriodDuration, newVelocityAtTarget, sequenceToPeriod, transitionSplines, velocityAtTarget, visit
    )

{-|

@docs Move, Movement, State, derivativeOfEasing

@docs Period

@docs startMoving

@docs coloring, moving, mapTransition

-}

import Color
import Duration
import Html.Events exposing (preventDefaultOn)
import Internal.Bezier as Bezier
import Internal.Move as Move
import Internal.Spring as Spring
import Internal.Time as Time
import Internal.Timeline as Timeline exposing (Period(..))
import Internal.Transition as Transition
import Internal.Units as Units
import Pixels
import Quantity


type alias Movement =
    Move Float


{-| -}
type alias Move value =
    Move.Move value


{-| -}
type Sequence value
    = Repeat Int (List (Step value))


type Step value
    = Step Duration.Duration Transition.Transition value
    | Wait Duration.Duration


sequenceToSplines : Time.Absolute -> Float -> Sequence Float -> List Bezier.Spline
sequenceToSplines startTime current seq =
    case seq of
        Repeat n steps ->
            stepsToSplines startTime current steps []


stepsToSplines time current steps splines =
    case steps of
        [] ->
            splines

        (Wait dur) :: remain ->
            let
                newTime =
                    time |> Time.advanceBy dur
            in
            stepsToSplines newTime
                current
                remain
                (Bezier.horizontal
                    (Time.inMilliseconds time)
                    (Time.inMilliseconds newTime)
                    current
                    :: splines
                )

        (Step dur trans value) :: remain ->
            let
                newTime =
                    time |> Time.advanceBy dur

                newSplines =
                    Transition.splines
                        { start =
                            { x = time
                            , y = Pixels.pixels current
                            }
                        , end =
                            { x = newTime
                            , y = Pixels.pixels value
                            }
                        }
                        Units.zero
                        Units.zero
                        trans
            in
            stepsToSplines
                (time |> Time.advanceBy dur)
                current
                remain
                (newSplines ++ splines)


sequenceToPeriod : Sequence value -> Timeline.Period
sequenceToPeriod seq =
    case seq of
        Repeat n _ ->
            if n == -1 then
                Timeline.Loop (getPeriodDuration seq)

            else
                Timeline.Repeat n (getPeriodDuration seq)


getIterationCssString : Sequence value -> String
getIterationCssString seq =
    case seq of
        Repeat 1 _ ->
            "1"

        Repeat n _ ->
            if n == -1 then
                "infinite"

            else
                String.fromInt n


getPeriodDuration : Sequence value -> Duration.Duration
getPeriodDuration seq =
    case seq of
        Repeat _ steps ->
            sumStepDuration steps Time.zeroDuration


sumStepDuration steps dur =
    case steps of
        [] ->
            dur

        (Step d _ _) :: remain ->
            sumStepDuration remain (Time.expand d dur)

        (Wait d) :: remain ->
            sumStepDuration remain (Time.expand d dur)


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


{-| Number betwen 0 and 1
-}
type alias Proportion =
    Float


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


startMoving : Movement -> State
startMoving movement =
    { position =
        case movement of
            Move.Pos _ x _ ->
                Pixels.pixels x
    , velocity = Pixels.pixelsPerSecond 0
    }


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
    , visit = visit
    , transition = transition
    }


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
    if Time.isZeroDuration dwellTime then
        { position =
            case lookup event of
                Move.Pos _ x _ ->
                    Pixels.pixels x
        , velocity =
            newVelocityAtTarget (lookup event)
                start
                maybeLookAhead
        }

    else
        case lookup event of
            Move.Pos _ pos [] ->
                { position = Pixels.pixels pos
                , velocity = Pixels.pixelsPerSecond 0
                }

            Move.Pos _ pos (seq :: _) ->
                -- Move.goto pos seq dwellTime
                { position = Pixels.pixels pos
                , velocity = Pixels.pixelsPerSecond 0
                }


type alias Milliseconds =
    Float


transitionSplines : Time.Absolute -> Movement -> Time.Absolute -> Maybe (Timeline.LookAhead Movement) -> State -> List Bezier.Spline
transitionSplines prevEndTime target targetTime maybeLookAhead state =
    let
        _ =
            Debug.log "    -> SPLINES"
                { start =
                    { x = Time.inMilliseconds prevEndTime
                    , y = Pixels.inPixels state.position
                    }
                , end =
                    { x = Time.inMilliseconds targetTime
                    , y = targetPos
                    }
                }

        targetPos =
            case target of
                Move.Pos _ x _ ->
                    x

        targetVelocity =
            newVelocityAtTarget target targetTime maybeLookAhead
    in
    Transition.splines
        { start =
            { x = prevEndTime
            , y = state.position
            }
        , end =
            { x = targetTime
            , y = Pixels.pixels targetPos
            }
        }
        state.velocity
        targetVelocity
        (case target of
            Move.Pos trans _ _ ->
                trans
        )


transition : Time.Absolute -> Movement -> Movement -> Time.Absolute -> Time.Absolute -> Maybe (Timeline.LookAhead Movement) -> State -> State
transition startTime prev2 target targetTime now maybeLookAhead state =
    let
        _ =
            Debug.log "DOMAIN POSITION"
                { start =
                    { x = Time.inMilliseconds startTime
                    , y = Pixels.inPixels state.position
                    }
                , end =
                    { x = Time.inMilliseconds targetTime
                    , y = Pixels.inPixels targetPosition
                    }
                }

        targetVelocity =
            newVelocityAtTarget target targetTime maybeLookAhead

        progress =
            Time.progress startTime targetTime now
                |> Debug.log "PROGRESS"

        targetPosition =
            case target of
                Move.Pos _ x _ ->
                    Pixels.pixels x
    in
    Transition.atX
        progress
        { start =
            { x = startTime
            , y = state.position
            }
        , end =
            { x = targetTime
            , y = targetPosition
            }
        }
        state.velocity
        targetVelocity
        (case target of
            Move.Pos trans _ _ ->
                trans
        )



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


sumDurations steps currentDur =
    case steps of
        [] ->
            currentDur

        (Wait dur) :: remain ->
            sumDurations remain (Time.expand currentDur dur)

        (Step dur _ _) :: remain ->
            sumDurations remain (Time.expand currentDur dur)


progressTowards total current =
    if total == 0 then
        0

    else
        (current / total)
            |> max 0
            |> min 1


sequenceSteps : Float -> List (Step Float) -> Duration.Duration -> Duration.Duration -> State
sequenceSteps previous steps durationTillNow durationCursor =
    case steps of
        [] ->
            { position = Pixels.pixels previous
            , velocity = Pixels.pixelsPerSecond 0
            }

        (Wait dur) :: remain ->
            let
                newDuration =
                    Time.expand durationCursor dur
            in
            if newDuration |> Quantity.greaterThan durationTillNow then
                { position = Pixels.pixels previous
                , velocity = Pixels.pixelsPerSecond 0
                }

            else
                sequenceSteps previous remain durationTillNow newDuration

        (Step dur trans val) :: remain ->
            let
                newDuration =
                    Time.expand durationCursor dur
            in
            if newDuration |> Quantity.greaterThan durationTillNow then
                let
                    progress =
                        durationTillNow
                            |> Quantity.minus durationCursor
                            |> Duration.inMilliseconds
                            |> progressTowards (Duration.inMilliseconds dur)
                in
                Transition.atX
                    progress
                    { start =
                        { x = Units.zero
                        , y = Pixels.pixels previous
                        }
                    , end =
                        { x = Quantity.Quantity 1
                        , y = Pixels.pixels val
                        }
                    }
                    Units.zero
                    Units.zero
                    trans

            else
                sequenceSteps val remain durationTillNow newDuration



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
                Move.Pos _ _ [] ->
                    zeroVelocity

                Move.Pos _ _ (seq :: _) ->
                    Move.initialSequenceVelocity seq

        Just lookAhead ->
            let
                targetPosition =
                    case target of
                        Move.Pos _ x _ ->
                            Pixels.pixels x
            in
            case lookAhead.anchor of
                Move.Pos _ aheadPosition [] ->
                    -- our target velocity is the linear velocity between target and lookahead
                    velocityBetween
                        targetPosition
                        targetTime
                        (Pixels.pixels aheadPosition)
                        lookAhead.time

                Move.Pos _ aheadPosition (seq :: _) ->
                    if lookAhead.resting then
                        Move.initialSequenceVelocity seq

                    else
                        velocityBetween
                            targetPosition
                            targetTime
                            (Pixels.pixels aheadPosition)
                            lookAhead.time


{-| -}
velocityAtTarget :
    (state -> Movement)
    -> Timeline.Occurring state
    -> List (Timeline.Occurring state)
    -> PixelsPerSecond
velocityAtTarget lookup target future =
    let
        movement =
            lookup (Timeline.getEvent target)
    in
    case future of
        [] ->
            case movement of
                Move.Pos _ _ [] ->
                    zeroVelocity

                Move.Pos _ _ (seq :: _) ->
                    Move.initialSequenceVelocity seq

        next :: _ ->
            let
                targetPosition =
                    case movement of
                        Move.Pos _ x _ ->
                            Pixels.pixels x
            in
            case lookup (Timeline.getEvent next) of
                Move.Pos _ aheadPosition [] ->
                    -- our target velocity is the linear velocity between target and lookahead
                    velocityBetween
                        targetPosition
                        (Timeline.endTime target)
                        (Pixels.pixels aheadPosition)
                        (Timeline.startTime next)

                Move.Pos _ aheadPosition (seq :: _) ->
                    if Timeline.isResting target then
                        Move.initialSequenceVelocity seq

                    else
                        velocityBetween
                            targetPosition
                            (Timeline.endTime target)
                            (Pixels.pixels aheadPosition)
                            (Timeline.startTime next)


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
    , visit =
        \lookup target now maybeLookAhead state ->
            lookup (Timeline.getEvent target)
    , transition = transitionColor
    }


transitionColor prevEndTime maybePrev target targetTime now maybeLookAhead state =
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
