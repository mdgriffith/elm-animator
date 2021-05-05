module Internal.Move exposing
    ( Move(..), to
    , Sequence
    , Step, step, stepWith, set
    , sequences, goto, continuingSplines, continue
    , css
    , initialSequenceVelocity, mapTransition
    )

{-|

@docs Move, to

@docs Sequence
@docs Step, step, stepWith, set

@docs sequences, goto, continuingSplines, continue

    type alias State =
        { position : Units.Pixels
        , velocity : Units.PixelsPerSecond
        }

    Needed:
        get position at transition
        get initial position
        get pos, vel given a dwell time

        velocity @ transition (i.e. velocity at target)


        sequences :
            -- start
            Time.Absolute
            -- target time
            -> Time.Absolute
            -- now
            -> Time.Absolute
            -> Move Float
            -> State
            ->
                { sequences : List Bezier.Spline
                , state : State
                }


        state :
            -- start
            Time.Absolute
            -- target time
            -> Time.Absolute
            -- now
            -> Time.Absolute
            -> Move Float
                State

@docs css

-}

import Duration
import Internal.Bezier as Bezier
import Internal.Time as Time
import Internal.Transition as Transition
import Internal.Units as Units
import Pixels
import Quantity


{-| -}
type Move value
    = Pos Transition.Transition value (List (Sequence value))


{-| -}
to : value -> Move value
to v =
    Pos Transition.standard v []


{-| -}
toWith : Transition.Transition -> value -> Move value
toWith t v =
    Pos t v []


{-| A sequence is something that can be easily

1.  rendered into a CSS keyframes
2.  combined with another sequence

We need to know:

1.  the full duration of the sequence so we can easily designate % for keyframe steps
2.  the exact durations for each step

Also, each `value` needs all information about how to get to the next `value`
which is the opposite of what elm-animator does.

-}
type Sequence value
    = Sequence Int Duration.Duration (List (Step value))


type Step value
    = Step Duration.Duration Transition.Transition value


type alias Pixels =
    Quantity.Quantity Float Pixels.Pixels


type alias PixelsPerSecond =
    Quantity.Quantity Float Pixels.PixelsPerSecond


set : value -> Step value
set =
    Step zeroDuration Transition.standard


step : Duration.Duration -> value -> Step value
step dur value =
    Step dur Transition.standard value


stepWith : Duration.Duration -> Transition.Transition -> value -> Step value
stepWith =
    Step


{--}
zeroVelocity : PixelsPerSecond
zeroVelocity =
    Pixels.pixelsPerSecond 0


type alias State =
    { position : Units.Pixels
    , velocity : Units.PixelsPerSecond
    }


{-| Transition finish velocity -> non-zero only if we're continuing on to another state

Will start providing Splines as soon as we pass `now`.

We are not continuing to another state, so we will report all of our splines.

    if now is after -> return nothing
    else if during -> split & rerepeat and return top

-}
sequences :
    -- start
    Time.Absolute
    -- target time
    -> Time.Absolute
    -- now
    -> Time.Absolute
    -> Move Float
    -> State
    -> List (Sequence Float)
    -> List (Sequence Float)
sequences startTime targetTime now movement state existingSequence =
    let
        durationToNow =
            Time.duration startTime now
    in
    if Time.thisAfterOrEqualThat startTime now then
        -- We've definitely started, so we want to report the full sequence
        -- mot common case will be startTime == now
        case movement of
            Pos trans value [] ->
                let
                    transitionDuration =
                        Time.duration startTime targetTime
                in
                Sequence 1 transitionDuration [ Step transitionDuration trans value ]
                    :: existingSequence

            Pos trans value [ Sequence 1 dur steps ] ->
                let
                    stepDuration =
                        Time.duration startTime targetTime

                    transitionDuration =
                        stepDuration
                            |> Time.expand dur
                in
                Sequence 1 transitionDuration (Step stepDuration trans value :: steps)
                    :: existingSequence

            Pos trans value dwell ->
                let
                    transitionDuration =
                        Time.duration startTime targetTime
                in
                Sequence 1 transitionDuration [ Step transitionDuration trans value ]
                    :: dwell
                    ++ existingSequence

    else if after startTime targetTime now movement then
        -- we've completely passed this state, no splines are returned
        []

    else
        -- now is during the new sequence
        -- so let's compose the new sequence and then split it at the new time
        -- we also know that existingSequence should be [] here
        case movement of
            Pos trans value [] ->
                let
                    splitTime =
                        Time.progress startTime targetTime now

                    transitionDuration =
                        Time.duration startTime targetTime

                    newSequence =
                        Sequence 1 transitionDuration [ Step transitionDuration trans value ]
                            |> takeAfter durationToNow
                in
                case newSequence.following of
                    Nothing ->
                        newSequence.base :: existingSequence

                    Just following ->
                        newSequence.base :: following :: existingSequence

            Pos trans value [ Sequence 1 dur steps ] ->
                let
                    stepDuration =
                        Time.duration startTime targetTime

                    transitionDuration =
                        stepDuration
                            |> Time.expand dur

                    new =
                        Sequence 1 transitionDuration (Step stepDuration trans value :: steps)
                            |> takeAfter durationToNow
                in
                case new.following of
                    Nothing ->
                        [ new.base ]

                    Just following ->
                        [ new.base, following ]

            Pos trans value dwell ->
                let
                    transitionDuration =
                        Time.duration startTime targetTime
                in
                if Time.thisAfterThat now targetTime then
                    takeAfterSequenceList durationToNow dwell

                else
                    let
                        new =
                            Sequence 1 transitionDuration [ Step transitionDuration trans value ]
                                |> takeAfter durationToNow
                    in
                    case new.following of
                        Nothing ->
                            new.base :: dwell

                        Just following ->
                            new.base :: following :: dwell


takeAfterSequenceList : Time.Duration -> List (Sequence value) -> List (Sequence value)
takeAfterSequenceList durationToNow seqs =
    case seqs of
        [] ->
            []

        ((Sequence n duration steps) as top) :: remain ->
            let
                floatN =
                    toFloat n
            in
            if isInfinite floatN then
                if duration |> Quantity.greaterThan durationToNow then
                    let
                        new =
                            takeAfter durationToNow top
                    in
                    case new.following of
                        Nothing ->
                            new.base :: remain

                        Just following ->
                            new.base :: following :: remain

                else
                    takeAfterSequenceList (durationToNow |> Quantity.minus duration) remain

            else
                let
                    fullSeqDuration =
                        duration |> Quantity.multiplyBy floatN
                in
                if fullSeqDuration |> Quantity.greaterThan durationToNow then
                    let
                        new =
                            takeAfter durationToNow top
                    in
                    case new.following of
                        Nothing ->
                            new.base :: remain

                        Just following ->
                            new.base :: following :: remain

                else
                    takeAfterSequenceList (durationToNow |> Quantity.minus fullSeqDuration)
                        remain


takeAfter : Time.Duration -> Sequence value -> { base : Sequence value, following : Maybe (Sequence value) }
takeAfter durationToNow ((Sequence n duration steps) as seq) =
    if durationToNow |> Quantity.lessThanOrEqualTo zeroDuration then
        { base = seq
        , following = Nothing
        }

    else
        let
            durationToNowMs =
                Duration.inMilliseconds durationToNow
                    |> round

            seqDurInMs =
                Duration.inMilliseconds duration
                    |> round

            newN =
                durationToNowMs
                    // seqDurInMs

            durationOfUnrolledSeq =
                durationToNowMs
                    |> modBy seqDurInMs
                    |> toFloat
                    |> Duration.milliseconds

            durationToNewStart =
                durationOfUnrolledSeq
        in
        { base =
            Sequence 1
                durationOfUnrolledSeq
                (takeStepsAfter (duration |> Quantity.minus durationOfUnrolledSeq) steps)
        , following =
            if newN - 1 <= 0 then
                Nothing

            else
                Just
                    (Sequence (newN - 1)
                        duration
                        steps
                    )
        }


takeStepsAfter durationToNow steps =
    case steps of
        [] ->
            []

        (Step duration transition value) :: remain ->
            if duration |> Quantity.greaterThan durationToNow then
                -- on this step
                let
                    progress =
                        Time.progressWithin durationToNow duration
                in
                Step (duration |> Quantity.minus durationToNow)
                    (Transition.takeAfter progress transition)
                    value
                    :: remain

            else
                takeStepsAfter (durationToNow |> Quantity.minus duration) remain


after startTime targetTime now movement =
    case movement of
        Pos _ _ [] ->
            Time.thisAfterThat now targetTime

        Pos _ _ (seq :: remaining) ->
            afterSequenceList (Time.duration startTime now) seq remaining


afterSequenceList durationTillNow seq remaining =
    if afterSequence durationTillNow seq then
        case remaining of
            [] ->
                True

            ((Sequence n duration steps) as top) :: rest ->
                let
                    durationOfUnrolledSeq =
                        duration |> Quantity.multiplyBy (toFloat n)
                in
                afterSequenceList
                    (durationTillNow |> Quantity.minus durationOfUnrolledSeq)
                    top
                    rest

    else
        False


afterSequence durationTillNow (Sequence n duration steps) =
    let
        floatN =
            toFloat n
    in
    if isInfinite floatN then
        False

    else
        let
            fullDuration =
                duration |> Quantity.multiplyBy floatN
        in
        durationTillNow |> Quantity.greaterThanOrEqualTo fullDuration


{-| This is the case where we specifically are transitioning to the new state and continuing to another state.

Will start providing Splines as soon as we pass `now`, otherwise we don't care about `now` as we know we're

-}
continuingSplines :
    -- start
    Time.Absolute
    -- target time
    -> Time.Absolute
    -- now
    -> Time.Absolute
    -> Units.PixelsPerSecond
    -> Move Float
    -> State
    -> List (Sequence Float)
continuingSplines startTime targetTime now transitionFinishVelocity movement state =
    Debug.todo "Move.splines"


{-| We are specifically going to a new state and not continuing on to another.

This means we report the state we are at `now`, including dwell.

-}
goto :
    -- start
    Time.Absolute
    -- target time
    -> Time.Absolute
    -- now
    -> Time.Absolute
    -> Move Float
    -> State
    -> State
goto startTime targetTime now movement state =
    Debug.todo "Move.goto"


{-| We're passing through this state enroute to somewhere else.
So, let's return the state @ targetTime.
-}
continue :
    -- start
    Time.Absolute
    -- target time
    -> Time.Absolute
    -- now
    -> Time.Absolute
    -> Units.PixelsPerSecond
    -> Move Float
    -> State
    -> State
continue startTime targetTime now transitionFinishVelocity movement state =
    Debug.todo "Move.goto"


{-| We are specifically going to a new state and not continuing on to another.

This means we report the state we are at `now`, including dwell.

-}
atStep :
    -- start
    Time.Absolute
    -- target time
    -> Time.Absolute
    -- now
    -> Time.Absolute
    -> Move value
    -> value
atStep startTime targetTime now movement =
    Debug.todo "Move.atStep"


{-| -}
stepsBetween :
    -- start
    Time.Absolute
    -- target time
    -> Time.Absolute
    -- now
    -> Time.Absolute
    -> Move value
    -> Sequence value
stepsBetween startTime targetTime now movement =
    Debug.todo "Move.stepsBetween"



-- initialVelocity : Move value -> Pixels.PixelsPerSecond
-- initialVelocity m =
--     case m of
--         Wait _ ->
--             zeroVelocity
--         To dur trans value ->
--             zeroVelocity
--         -- duration is gathered from the timeline
--         -- otherwise it's 0
--         Transition trans value ->
--             zeroVelocity
--         Repeat n steps ->
--             zeroVelocity
{- SEQUENCE -}
-- {-| -}
-- sequence :
--     Float
--     -> Sequence Float
--     -> Duration.Duration
--     -> State
-- sequence start seq durationTillNow =
--     let
--         sequencePeriodDuration =
--             case seq of
--                 Repeat _ steps ->
--                     sumDurations steps zeroDuration
--         normalizedDurationTillNow =
--             case seq of
--                 Repeat n _ ->
--                     if n == 0 then
--                         durationTillNow
--                     else if n < 0 then
--                         -- this means infinite
--                         let
--                             iterations =
--                                 Duration.inMilliseconds durationTillNow
--                                     / Duration.inMilliseconds sequencePeriodDuration
--                         in
--                         durationTillNow
--                             |> Quantity.minus
--                                 (Quantity.multiplyBy iterations sequencePeriodDuration)
--                     else
--                         Duration.inMilliseconds durationTillNow
--                             |> round
--                             |> modBy (round (Duration.inMilliseconds sequencePeriodDuration))
--                             |> toFloat
--                             |> Duration.milliseconds
--     in
--     case seq of
--         Repeat _ [] ->
--             { position = Pixels.pixels start
--             , velocity = Pixels.pixelsPerSecond 0
--             }
--         Repeat 0 _ ->
--             { position = Pixels.pixels start
--             , velocity = Pixels.pixelsPerSecond 0
--             }
--         Repeat n steps ->
--             if n < 1 then
--                 { position = Pixels.pixels start
--                 , velocity = Pixels.pixelsPerSecond 0
--                 }
--             else
--                 sequenceSteps start steps normalizedDurationTillNow zeroDuration
-- sumDurations steps currentDur =
--     case steps of
--         [] ->
--             currentDur
--         (Wait dur) :: remain ->
--             sumDurations remain (Time.expand currentDur dur)
--         (Step dur _ _) :: remain ->
--             sumDurations remain (Time.expand currentDur dur)
-- progressTowards total current =
--     if total == 0 then
--         0
--     else
--         (current / total)
--             |> max 0
--             |> min 1
-- sequenceSteps : Float -> List (Step Float) -> Duration.Duration -> Duration.Duration -> State
-- sequenceSteps previous steps durationTillNow durationCursor =
--     case steps of
--         [] ->
--             { position = Pixels.pixels previous
--             , velocity = Pixels.pixelsPerSecond 0
--             }
--         (Wait dur) :: remain ->
--             let
--                 newDuration =
--                     Time.expand durationCursor dur
--             in
--             if newDuration |> Quantity.greaterThan durationTillNow then
--                 { position = Pixels.pixels previous
--                 , velocity = Pixels.pixelsPerSecond 0
--                 }
--             else
--                 sequenceSteps previous remain durationTillNow newDuration
--         (Step dur trans val) :: remain ->
--             let
--                 newDuration =
--                     Time.expand durationCursor dur
--             in
--             if newDuration |> Quantity.greaterThan durationTillNow then
--                 let
--                     progress =
--                         durationTillNow
--                             |> Quantity.minus durationCursor
--                             |> Duration.inMilliseconds
--                             |> progressTowards (Duration.inMilliseconds dur)
--                 in
--                 Transition.atX
--                     progress
--                     { start =
--                         { x = Units.zero
--                         , y = Pixels.pixels previous
--                         }
--                     , end =
--                         { x = Quantity.Quantity 1
--                         , y = Pixels.pixels val
--                         }
--                     }
--                     Units.zero
--                     Units.zero
--                     trans
--             else
--                 sequenceSteps val remain durationTillNow newDuration


zeroDuration : Duration.Duration
zeroDuration =
    Duration.milliseconds 0



{- CSS KEYFRAMES -}


push : Sequence value -> List (Sequence value) -> List (Sequence value)
push top stack =
    case top of
        Sequence 1 topDuration topSteps ->
            case stack of
                [] ->
                    top :: stack

                (Sequence 1 seqDuration steps) :: others ->
                    let
                        transitionDuration =
                            topDuration
                                |> Time.expand seqDuration
                    in
                    Sequence 1 transitionDuration (steps ++ topSteps) :: others

                _ ->
                    top :: stack

        nonMergable ->
            nonMergable :: stack


mapTransition fn movement =
    case movement of
        Pos p f seq ->
            Pos (fn p) f seq


initialSequenceVelocity : Sequence value -> PixelsPerSecond
initialSequenceVelocity seq =
    case seq of
        Sequence 0 _ _ ->
            zeroVelocity

        Sequence _ _ [] ->
            zeroVelocity

        Sequence n _ ((Step dur trans _) :: _) ->
            Transition.initialVelocity trans
                |> (*) 1000
                |> Pixels.pixelsPerSecond


keyframes : String -> (Float -> String) -> Sequence Float -> String
keyframes name toString seq =
    ""


hash : String -> Sequence Float -> String
hash name seq =
    ""


css delay name toString seq =
    let
        animationName =
            hash name seq

        n =
            case seq of
                Sequence i _ _ ->
                    if i <= 0 then
                        "1"

                    else if isInfinite (toFloat i) then
                        "infinite"

                    else
                        String.fromInt i

        durationStr =
            case seq of
                Sequence _ dur _ ->
                    dur
                        |> Duration.inMilliseconds
                        |> round
                        |> String.fromInt
                        |> (\s -> s ++ "ms")

        delayStr =
            delay
                |> Duration.inMilliseconds
                |> round
                |> String.fromInt
                |> (\s -> s ++ "ms")
    in
    { hash = animationName
    , animation =
        (durationStr ++ " ")
            -- we specify an easing function here because it we have to
            -- , but it is overridden by the one in keyframes
            ++ "linear "
            ++ delayStr
            ++ " "
            ++ n
            ++ " normal forwards running "
            ++ animationName
    , keyframes =
        ("@keyframes " ++ animationName ++ " {\n")
            ++ keyframes name toString seq
            ++ "\n}"
    , props = []
    }



-- {-| -}
-- sequence :
--     Float
--     -> Sequence Float
--     -> Duration.Duration
--     -> State
-- sequence start seq durationTillNow =
--     let
--         sequencePeriodDuration =
--             case seq of
--                 Repeat _ steps ->
--                     sumDurations steps zeroDuration
--         normalizedDurationTillNow =
--             case seq of
--                 Repeat n _ ->
--                     if n == 0 then
--                         durationTillNow
--                     else if n < 0 then
--                         -- this means infinite
--                         let
--                             iterations =
--                                 Duration.inMilliseconds durationTillNow
--                                     / Duration.inMilliseconds sequencePeriodDuration
--                         in
--                         durationTillNow
--                             |> Quantity.minus
--                                 (Quantity.multiplyBy iterations sequencePeriodDuration)
--                     else
--                         Duration.inMilliseconds durationTillNow
--                             |> round
--                             |> modBy (round (Duration.inMilliseconds sequencePeriodDuration))
--                             |> toFloat
--                             |> Duration.milliseconds
--     in
--     case seq of
--         Repeat _ [] ->
--             { position = Pixels.pixels start
--             , velocity = Pixels.pixelsPerSecond 0
--             }
--         Repeat 0 _ ->
--             { position = Pixels.pixels start
--             , velocity = Pixels.pixelsPerSecond 0
--             }
--         Repeat n steps ->
--             if n < 1 then
--                 { position = Pixels.pixels start
--                 , velocity = Pixels.pixelsPerSecond 0
--                 }
--             else
--                 sequenceSteps start steps normalizedDurationTillNow zeroDuration
