module Internal.Move exposing
    ( Move(..), to
    , Sequence
    , Step, step, stepWith, set
    , sequences, goto, continuingSplines, continue
    , css
    , atX, initialSequenceVelocity, lastPosOr, mapTransition, normalizeOver, toReal, withVelocities
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


{-| -}
to : value -> Move value
to v =
    Pos Transition.standard v []


{-| -}
toWith : Transition.Transition -> value -> Move value
toWith t v =
    Pos t v []


getSequenceDuration : Sequence value -> Duration.Duration
getSequenceDuration (Sequence i dur steps) =
    dur


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


addDelayToSequence :
    Duration.Duration
    -> List (Sequence Float)
    ->
        List
            { delay : Duration.Duration
            , sequence : Sequence Float
            }
    ->
        List
            { delay : Duration.Duration
            , sequence : Sequence Float
            }
addDelayToSequence delay seqs captured =
    case seqs of
        [] ->
            List.reverse captured

        ((Sequence 1 sequenceDuration steps) as seq) :: remain ->
            addDelayToSequence (Time.expand delay sequenceDuration)
                remain
                ({ sequence = seq
                 , delay = delay
                 }
                    :: captured
                )

        ((Sequence n sequenceDuration steps) as seq) :: remain ->
            addDelayToSequence (Time.expand delay sequenceDuration)
                remain
                ({ sequence = seq
                 , delay = delay
                 }
                    :: captured
                )


{-|

    Adjust the transition by taking into account intro and exit velocity if necessary

-}
withVelocities : Float -> Float -> Move x -> Move x
withVelocities intro exit ((Pos trans val dwell) as untouched) =
    if intro == 0 && exit == 0 then
        untouched

    else
        Pos
            (Transition.withVelocities intro exit trans)
            val
            dwell


atX :
    Float
    -> Move Float
    ->
        { position : Bezier.Point
        , velocity : Bezier.Point
        }
atX progress (Pos trans value dwell) =
    Transition.atX2 progress trans


{-|

    Map a value to 0:1 given a range it should be in.

-}
normalizeOver : Float -> Float -> Float -> Float
normalizeOver start end current =
    let
        total =
            abs (end - start)
    in
    if total == 0 then
        0

    else
        ((current - start) / total)
            |> max 0
            |> min 1


{-| The opposite of `normalizeOver`.

I guess this is denormalization? Though i was always confused by that term :/

-}
toReal : Float -> Float -> Float -> Float
toReal start end t =
    start + (t * (end - start))


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
    -- stop time
    -> Time.Absolute
    -> Move Float
    ->
        List
            { delay : Duration.Duration
            , sequence : Sequence Float
            }
    ->
        List
            { delay : Duration.Duration
            , sequence : Sequence Float
            }
sequences startTime targetTime now stopTime movement existingSequence =
    let
        durationToNow =
            Time.duration startTime now
    in
    if Time.equal now stopTime then
        -- We've probably been interrupted
        []

    else if Time.thisAfterOrEqualThat startTime now then
        -- We've definitely started, so we want to report the full sequence
        -- most common case will be startTime == now
        case movement of
            Pos trans value [] ->
                let
                    transitionDuration =
                        Time.duration startTime targetTime

                    seq =
                        Sequence 1
                            transitionDuration
                            [ Step transitionDuration trans value
                            ]
                in
                { sequence = seq
                , delay = durationToNow
                }
                    :: existingSequence

            Pos trans value [ Sequence 1 dur steps ] ->
                let
                    stepDuration =
                        Time.duration startTime targetTime

                    transitionDuration =
                        stepDuration
                            |> Time.expand dur

                    transitionSequence =
                        Sequence 1
                            transitionDuration
                            (Step stepDuration trans value
                                :: steps
                            )
                in
                { sequence = transitionSequence
                , delay = durationToNow
                }
                    :: existingSequence

            Pos trans value dwell ->
                let
                    transitionDuration =
                        Time.duration startTime targetTime

                    transitionSequence =
                        Sequence 1
                            transitionDuration
                            [ Step transitionDuration trans value
                            ]
                in
                { sequence = transitionSequence
                , delay = durationToNow
                }
                    :: addDelayToSequence (Time.expand durationToNow transitionDuration) dwell []
                    ++ existingSequence

    else if after startTime stopTime now movement then
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
                        [ { sequence = newSequence.base, delay = Time.zeroDuration } ]

                    Just following ->
                        [ { sequence = newSequence.base
                          , delay = Time.zeroDuration
                          }
                        , { sequence = following
                          , delay = getSequenceDuration newSequence.base
                          }
                        ]

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
                        [ { sequence = new.base
                          , delay = Time.zeroDuration
                          }
                        ]

                    Just following ->
                        [ { sequence = new.base
                          , delay = Time.zeroDuration
                          }
                        , { sequence = following
                          , delay = getSequenceDuration new.base
                          }
                        ]

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
                            { sequence = new.base, delay = Time.zeroDuration }
                                :: addDelayToSequence (getSequenceDuration new.base) dwell []

                        Just following ->
                            let
                                delayToFollowing =
                                    getSequenceDuration new.base
                            in
                            { sequence = new.base
                            , delay = Time.zeroDuration
                            }
                                :: { sequence = following
                                   , delay = delayToFollowing
                                   }
                                :: addDelayToSequence
                                    (Time.expand (getSequenceDuration following)
                                        delayToFollowing
                                    )
                                    dwell
                                    []


takeAfterSequenceList :
    Time.Duration
    -> List (Sequence Float)
    ->
        List
            { delay : Duration.Duration
            , sequence : Sequence Float
            }
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
                            { sequence = new.base
                            , delay = Time.zeroDuration
                            }
                                :: addDelayToSequence
                                    (getSequenceDuration new.base)
                                    remain
                                    []

                        Just following ->
                            let
                                delayToFollowing =
                                    getSequenceDuration new.base
                            in
                            { sequence = new.base
                            , delay = Time.zeroDuration
                            }
                                :: { sequence = following
                                   , delay = delayToFollowing
                                   }
                                :: addDelayToSequence
                                    (Time.expand
                                        (getSequenceDuration following)
                                        delayToFollowing
                                    )
                                    remain
                                    []

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
                            { sequence = new.base
                            , delay = Time.zeroDuration
                            }
                                :: addDelayToSequence
                                    (getSequenceDuration new.base)
                                    remain
                                    []

                        Just following ->
                            let
                                delayToFollowing =
                                    getSequenceDuration new.base
                            in
                            { sequence = new.base
                            , delay = Time.zeroDuration
                            }
                                :: { sequence = following
                                   , delay = delayToFollowing
                                   }
                                :: addDelayToSequence
                                    (Time.expand
                                        (getSequenceDuration following)
                                        delayToFollowing
                                    )
                                    remain
                                    []

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

            remainingDuration =
                duration
                    |> Quantity.minus durationOfUnrolledSeq
        in
        { base =
            Sequence 1
                remainingDuration
                (takeStepsAfter durationOfUnrolledSeq steps)
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


after : Time.Absolute -> Time.Absolute -> Time.Absolute -> Move value -> Bool
after startTime stopTime now movement =
    case movement of
        Pos _ _ [] ->
            Time.thisAfterThat now stopTime

        Pos _ _ (seq :: remaining) ->
            afterSequenceList (Time.duration startTime now) seq remaining


afterSequenceList :
    Quantity.Quantity Float Duration.Seconds
    -> Sequence value
    -> List (Sequence value)
    -> Bool
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

Will start providing Splines as soon as we pass `now`.

We also know that we don't care about dwell sequences in this case because we're immediately coninuing on to another state.

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
    ->
        List
            { delay : Duration.Duration
            , sequence : Sequence Float
            }
    ->
        List
            { delay : Duration.Duration
            , sequence : Sequence Float
            }
continuingSplines startTime targetTime now transitionFinishVelocity movement state existingSequence =
    let
        durationToNow =
            Time.duration startTime now
    in
    if Time.thisAfterOrEqualThat startTime now then
        -- We've definitely started, so we want to report the full sequence
        -- mot common case will be startTime == now
        case movement of
            Pos trans value _ ->
                let
                    transitionDuration =
                        Time.duration startTime targetTime

                    seq =
                        Sequence 1
                            transitionDuration
                            [ Step transitionDuration trans value ]
                in
                { sequence = seq
                , delay = durationToNow
                }
                    :: existingSequence

    else if after startTime targetTime now movement then
        -- we've completely passed this state, no splines are returned
        []

    else
        -- now is during the new sequence
        -- so let's compose the new sequence and then split it at the new time
        -- we also know that existingSequence should be [] here
        case movement of
            Pos trans value _ ->
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
                        [ { sequence = newSequence.base, delay = Time.zeroDuration } ]

                    Just following ->
                        [ { sequence = newSequence.base
                          , delay = Time.zeroDuration
                          }
                        , { sequence = following
                          , delay = getSequenceDuration newSequence.base
                          }
                        ]


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


keyframes : String -> Float -> (Float -> String) -> Sequence Float -> String -> String
keyframes name startPos toString (Sequence _ dur steps) rendered =
    keyframeHelper name
        startPos
        toString
        dur
        zeroDuration
        steps
        rendered


keyframeHelper :
    String
    -> Float
    -> (Float -> String)
    -> Time.Duration
    -> Time.Duration
    -> List (Step Float)
    -> String
    -> String
keyframeHelper name startPos toString sequenceDuration currentDur steps rendered =
    case steps of
        [] ->
            rendered

        (Step dur transition val) :: [] ->
            let
                toPropString v =
                    name ++ ":" ++ toString v

                last =
                    "100% { " ++ (toPropString val ++ ";}")

                domain =
                    { start =
                        { x = 0
                        , y = startPos
                        }
                    , end =
                        { x = 100
                        , y = val
                        }
                    }

                startPercent =
                    Time.progressWithin currentDur sequenceDuration * 100

                endPercent =
                    100

                frames =
                    Transition.keyframes
                        domain
                        startPercent
                        endPercent
                        toPropString
                        transition
            in
            rendered ++ frames ++ last

        (Step dur transition val) :: (((Step nextdur nextTrans nextVal) :: future) as remaining) ->
            let
                domain =
                    { start =
                        { x = 0
                        , y = startPos
                        }
                    , end =
                        { x = 100
                        , y = val
                        }
                    }

                nextCurrent =
                    Time.expand currentDur dur

                startPercent =
                    Time.progressWithin currentDur sequenceDuration * 100

                endPercent =
                    Time.progressWithin nextCurrent sequenceDuration * 100

                frames =
                    Transition.keyframes
                        domain
                        startPercent
                        endPercent
                        toString
                        transition
            in
            keyframeHelper
                name
                val
                toString
                sequenceDuration
                nextCurrent
                remaining
                (rendered ++ frames)


hash : String -> Sequence Float -> String
hash name (Sequence n dur steps) =
    name ++ String.fromInt n ++ stepHash steps ""


stepHash : List (Step Float) -> String -> String
stepHash steps hashed =
    case steps of
        [] ->
            hashed

        (Step dur trans v) :: remain ->
            stepHash remain
                (hashed
                    ++ "--"
                    ++ hashDuration dur
                    ++ "-"
                    ++ Transition.hash trans
                    ++ "-"
                    ++ floatToString v
                )


roundFloat : Float -> Float
roundFloat f =
    toFloat (round (f * 100)) / 100


floatToString : Float -> String
floatToString f =
    String.fromFloat (roundFloat f)


hashDuration : Duration.Duration -> String
hashDuration dur =
    String.fromInt
        (round (Duration.inSeconds dur))


lastPosOr : value -> Sequence value -> value
lastPosOr x (Sequence _ _ steps) =
    case steps of
        [] ->
            x

        _ ->
            lastPosOrHelper x steps


lastPosOrHelper : value -> List (Step value) -> value
lastPosOrHelper x steps =
    case steps of
        [] ->
            x

        (Step _ _ v) :: [] ->
            v

        (Step _ _ _) :: (Step _ _ v) :: [] ->
            v

        (Step _ _ _) :: (Step _ _ _) :: (Step _ _ v) :: [] ->
            v

        (Step _ _ _) :: (Step _ _ _) :: (Step _ _ _) :: (Step _ _ v) :: [] ->
            v

        (Step _ _ _) :: (Step _ _ _) :: (Step _ _ _) :: (Step _ _ v) :: remain ->
            lastPosOrHelper v remain


css :
    Time.Absolute
    -> Duration.Duration
    -> Float
    -> String
    -> (Float -> String)
    -> Sequence Float
    ->
        { hash : String
        , animation : String
        , keyframes : String
        , props : List a
        }
css now delay startPos name toString seq =
    let
        animationName =
            -- we need to encode the current time in the animations name so the browser doesn't cache anything
            -- IM LOOKIN AT YOU, CHROME
            hash (name ++ String.fromInt (round <| Time.inMilliseconds now)) seq

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
            ++ keyframes name startPos toString seq ""
            ++ "\n}"
    , props = []
    }
