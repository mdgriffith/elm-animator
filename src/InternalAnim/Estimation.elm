module InternalAnim.Estimation exposing (velocity, velocityAtTarget)

{-| -}

import Animator
import Animator.Timeline
import Animator.Value
import InternalAnim.Duration as Duration
import InternalAnim.Move as Move
import InternalAnim.Quantity as Quantity
import InternalAnim.Time as Time
import InternalAnim.Timeline as Timeline
import InternalAnim.Units as Units exposing (Pixels, PixelsPerSecond)
import Time


mapTime fn time =
    Time.millisToPosix (fn (Time.posixToMillis time))


{-| Estimate velocity in pixels/second
-}
velocity : Int -> Time.Posix -> Animator.Timeline.Timeline event -> (event -> Animator.Value.Movement) -> Float
velocity resolution time timeline toPosition =
    let
        before =
            mapTime (\t -> t - resolution) time

        after =
            mapTime (\t -> t + resolution) time

        zero =
            Animator.Value.movement (Timeline.atTime before timeline) toPosition

        one =
            Animator.Value.movement (Timeline.atTime time timeline) toPosition

        two =
            Animator.Value.movement (Timeline.atTime after timeline) toPosition

        first =
            (one.position - zero.position) / toFloat resolution

        second =
            (two.position - one.position) / toFloat resolution

        expected =
            -- 1000 * avg first second
            1000 * (two.position - zero.position) / (2 * toFloat resolution)
    in
    expected


{-| -}
velocityAtTarget :
    (state -> Move.Move Float)
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
                            Units.pixels x
            in
            case lookup (Timeline.getEvent next) of
                Move.Pos _ aheadPosition [] ->
                    -- our target velocity is the linear velocity between target and lookahead
                    velocityBetween
                        targetPosition
                        (Timeline.endTime target)
                        (Units.pixels aheadPosition)
                        (Timeline.startTime next)

                Move.Pos _ aheadPosition (seq :: _) ->
                    if Timeline.isResting target then
                        Move.initialSequenceVelocity seq

                    else
                        velocityBetween
                            targetPosition
                            (Timeline.endTime target)
                            (Units.pixels aheadPosition)
                            (Timeline.startTime next)


velocityBetween :
    Pixels
    -> Time.Absolute
    -> Pixels
    -> Time.Absolute
    -> PixelsPerSecond
velocityBetween one oneTime two twoTime =
    let
        distance =
            two
                |> Quantity.minus one

        duration =
            Time.duration oneTime twoTime

        vel =
            Units.inPixels distance
                / Duration.inSeconds duration
    in
    if isNaN vel || isInfinite vel then
        Quantity.zero

    else
        Units.pixelsPerSecond vel


zeroVelocity : PixelsPerSecond
zeroVelocity =
    Units.pixelsPerSecond 0
