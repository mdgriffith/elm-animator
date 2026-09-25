module InternalAnim.Move exposing
    ( Move(..), to, toWith
    , toValue, toTransition
    , State, init, toState
    , lerpColor
    , Sequence(..)
    , Step(..)
    , sequences
    , withTransition
    , at
    )

{-|

@docs Move, to, toWith

@docs toValue, toTransition

@docs State, init, toState

@docs lerpColor

@docs Sequence
@docs Step

@docs sequences

@docs withTransition

@docs at

-}

import Color
import InternalAnim.Duration as Duration
import InternalAnim.Quantity as Quantity
import InternalAnim.Time as Time
import InternalAnim.Transition as Transition
import InternalAnim.Units as Units


{-| -}
type Move value
    = Pos Transition.Transition value (List (Sequence value))


toValue : Move value -> value
toValue (Pos _ value _) =
    value


toTransition : Move value -> Transition.Transition
toTransition (Pos trans _ _) =
    trans


{-| A sequence is something that can be easily

1.  rendered into a CSS keyframes
2.  combined with another sequence

We need to know:

1.  the full duration of the sequence so we can easily designate % for keyframe steps
2.  the exact durations for each step

Also, each `value` needs all information about how to get to the next `value`
which is the opposite of what elm-animator does.

    --         repeat, delay,        duration



-}
type Sequence value
    = Sequence Int Duration.Duration Duration.Duration (List (Step value))


type Step value
    = Step Duration.Duration Transition.Transition value


init : Move Float -> State
init movement =
    { position =
        case movement of
            Pos _ x _ ->
                Units.pixels x
    , velocity = Units.pixelsPerSecond 0
    }


toState : Float -> State
toState x =
    { position =
        Units.pixels x
    , velocity = Units.pixelsPerSecond 0
    }


{-| -}
withTransition : Transition.Transition -> Move value -> Move value
withTransition trans (Pos _ value sequence) =
    Pos
        trans
        value
        sequence


{-| -}
to : value -> Move value
to v =
    Pos Transition.standard v []


{-| -}
toWith : Transition.Transition -> value -> Move value
toWith t v =
    Pos t v []


getSequenceDuration : Sequence value -> Duration.Duration
getSequenceDuration (Sequence i delay dur steps) =
    dur


withSequenceDelay : Duration.Duration -> Sequence value -> Sequence value
withSequenceDelay delay (Sequence i _ dur steps) =
    Sequence i delay dur steps


{--}
type alias State =
    { position : Units.Pixels
    , velocity : Units.PixelsPerSecond
    }


addDelayToSequence :
    Duration.Duration
    -> List (Sequence value)
    -> List (Sequence value)
    -> List (Sequence value)
addDelayToSequence delay seqs captured =
    case seqs of
        [] ->
            List.reverse captured

        seq :: remain ->
            addDelayToSequence (Time.expand delay (getSequenceDuration seq))
                remain
                (push (seq |> withSequenceDelay delay)
                    captured
                )


lerpColor : Float -> Color.Color -> Color.Color -> Color.Color
lerpColor progress from target =
    let
        one =
            Color.toRgba from

        two =
            Color.toRgba target
    in
    Color.rgba
        (average one.red two.red progress)
        (average one.green two.green progress)
        (average one.blue two.blue progress)
        (average one.alpha two.alpha progress)


average : Float -> Float -> Float -> Float
average x y progress =
    sqrt ((x * x) * (1 - progress) + (y * y) * progress)


at :
    Float
    -> Time.Absolute
    -> Time.Absolute
    -> Move Float
    -> State
    -> State
at progress startTime targetTime (Pos transition targetPosition dwell) current =
    if progress >= 1 then
        toState targetPosition

    else
        Transition.atX progress startTime targetTime transition current targetPosition


{-| Adds a new sequence to the top of the sequence stack.

If possible, will combine with the previous stack.

Sequences are combineable when:

1.  There is no delay between them (top delay == existing sequence duration)
2.  They have the same repeat value

-}
push : Sequence value -> List (Sequence value) -> List (Sequence value)
push ((Sequence topI topDelay topDuration topSteps) as top) stack =
    case stack of
        [] ->
            top :: stack

        (Sequence seqI seqDelay seqDuration steps) :: others ->
            if topI - seqI == 0 && Quantity.equalWithin (Quantity.Quantity 0.1) topDelay seqDuration then
                let
                    transitionDuration =
                        Time.expand seqDuration topDuration
                in
                Sequence seqI seqDelay transitionDuration (steps ++ topSteps) :: others

            else
                top :: stack


append : List (Sequence value) -> List (Sequence value) -> List (Sequence value)
append first stack =
    case first of
        [] ->
            stack

        [ top ] ->
            push top stack

        _ ->
            first ++ stack


{-| Will start providing Splines as soon as we pass `now`.

We are not continuing to another state, so we will report all of our splines.

    if now is after -> return nothing
    else if during -> split & rerepeat and return top

Start Time - When the transition starts
Target time -
Now - The current time
Stop time -

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
    -> Move value
    -> List (Sequence value)
    -> List (Sequence value)
sequences startTime targetTime now stopTime movement existingSequence =
    if Time.equal now stopTime && not (Time.equal targetTime stopTime) then
        -- We've probably been interrupted
        []

    else
        let
            durationToNow =
                Time.duration startTime now
        in
        if Time.thisAfterOrEqualThat startTime now then
            -- We've definitely started, so we want to report the full sequence
            -- most common case will be startTime == now
            case movement of
                Pos trans value [] ->
                    let
                        transitionDuration =
                            Time.duration startTime targetTime

                        seq =
                            Sequence 1
                                durationToNow
                                transitionDuration
                                [ Step transitionDuration trans value
                                ]
                    in
                    push seq existingSequence

                Pos trans value [ Sequence 1 delay dur steps ] ->
                    let
                        stepDuration =
                            Time.duration startTime targetTime

                        transitionDuration =
                            stepDuration
                                |> Time.expand dur

                        transitionSequence =
                            Sequence 1
                                (Time.expand durationToNow delay)
                                transitionDuration
                                (Step stepDuration trans value
                                    :: steps
                                )
                    in
                    push transitionSequence
                        existingSequence

                Pos trans value dwell ->
                    let
                        transitionDuration =
                            Time.duration startTime targetTime
                    in
                    if Time.isZeroDuration transitionDuration && not (List.isEmpty dwell) then
                        existingSequence
                            |> append (addDelayToSequence durationToNow dwell [])

                    else
                        let
                            transitionSequence =
                                Sequence 1
                                    durationToNow
                                    transitionDuration
                                    [ Step transitionDuration trans value
                                    ]
                        in
                        existingSequence
                            |> push transitionSequence
                            |> append (addDelayToSequence (Time.expand durationToNow transitionDuration) dwell [])

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
                        transitionDuration =
                            Time.duration startTime targetTime

                        newSequence =
                            Sequence 1 Quantity.zero transitionDuration [ Step transitionDuration trans value ]
                                |> takeAfter durationToNow
                    in
                    case newSequence.following of
                        Nothing ->
                            [ newSequence.base ]

                        Just following ->
                            [ newSequence.base ]
                                |> push
                                    (following
                                        |> withSequenceDelay (getSequenceDuration newSequence.base)
                                    )

                Pos trans value [ Sequence 1 _ dur steps ] ->
                    let
                        stepDuration =
                            Time.duration startTime targetTime

                        transitionDuration =
                            stepDuration
                                |> Time.expand dur

                        new =
                            Sequence 1 Quantity.zero transitionDuration (Step stepDuration trans value :: steps)
                                |> takeAfter durationToNow
                    in
                    case new.following of
                        Nothing ->
                            [ new.base
                            ]

                        Just following ->
                            [ new.base ]
                                |> push
                                    (following |> withSequenceDelay (getSequenceDuration new.base))

                Pos trans value dwell ->
                    if Time.thisAfterThat now targetTime then
                        takeAfterSequenceList durationToNow dwell

                    else
                        let
                            transitionDuration =
                                Time.duration startTime targetTime

                            new =
                                Sequence 1 Quantity.zero transitionDuration [ Step transitionDuration trans value ]
                                    |> takeAfter durationToNow
                        in
                        case new.following of
                            Nothing ->
                                new.base
                                    :: addDelayToSequence (getSequenceDuration new.base) dwell []

                            Just following ->
                                let
                                    delayToFollowing =
                                        getSequenceDuration new.base
                                in
                                [ new.base ]
                                    |> push (following |> withSequenceDelay delayToFollowing)
                                    |> append
                                        (addDelayToSequence
                                            (Time.expand (getSequenceDuration following)
                                                delayToFollowing
                                            )
                                            dwell
                                            []
                                        )


takeAfterSequenceList :
    Time.Duration
    -> List (Sequence value)
    -> List (Sequence value)
takeAfterSequenceList durationToNow seqs =
    case seqs of
        [] ->
            []

        ((Sequence n _ duration _) as top) :: remain ->
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
                            new.base
                                :: addDelayToSequence
                                    (getSequenceDuration new.base)
                                    remain
                                    []

                        Just following ->
                            let
                                delayToFollowing =
                                    getSequenceDuration new.base
                            in
                            new.base
                                :: (following |> withSequenceDelay delayToFollowing)
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
                            new.base
                                :: addDelayToSequence
                                    (getSequenceDuration new.base)
                                    remain
                                    []

                        Just following ->
                            let
                                delayToFollowing =
                                    getSequenceDuration new.base
                            in
                            new.base
                                :: (following |> withSequenceDelay delayToFollowing)
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
takeAfter durationToNow ((Sequence n delay duration steps) as seq) =
    if durationToNow |> Quantity.lessThanOrEqualTo zeroDuration then
        { base = seq
        , following = Nothing
        }

    else if durationToNow |> Quantity.lessThanOrEqualTo delay then
        { base = Sequence n (delay |> Quantity.minus durationToNow) duration steps
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
                Quantity.zero
                remainingDuration
                (takeStepsAfter durationOfUnrolledSeq steps)
        , following =
            if newN - 1 <= 0 then
                Nothing

            else
                Just
                    (Sequence (newN - 1)
                        remainingDuration
                        duration
                        steps
                    )
        }


takeStepsAfter :
    Time.Duration
    -> List (Step value)
    -> List (Step value)
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
    Duration.Duration
    -> Sequence value
    -> List (Sequence value)
    -> Bool
afterSequenceList durationTillNow seq remaining =
    if afterSequence durationTillNow seq then
        case remaining of
            [] ->
                True

            ((Sequence n _ duration _) as top) :: rest ->
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


afterSequence : Duration.Duration -> Sequence value -> Bool
afterSequence durationTillNow (Sequence n delay duration steps) =
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


zeroDuration : Duration.Duration
zeroDuration =
    Duration.milliseconds 0
