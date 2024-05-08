module InternalAnim.Move exposing
    ( Move(..), to, toWith
    , State, init
    , lerpColor, lerpFloat, lerpVector
    , Sequence(..)
    , Step(..), stepWith
    , sequences, hasSequence
    , addSequence, cssForSections, AllowTransitions(..)
    , withTransition, withVelocities
    , at
    , denormalize
    , move, toState
    )

{-|

@docs Move, to, toWith

@docs State, init, fromFloat

@docs lerpColor, lerpFloat, lerpVector

@docs Sequence
@docs Step, stepWith

@docs sequences, goto, hasSequence

@docs addSequence, cssForSections, AllowTransitions

@docs withTransition, withVelocities

@docs at

@docs denormalize

-}

import Bezier
import Color
import InternalAnim.Duration as Duration
import InternalAnim.Quantity as Quantity
import InternalAnim.Time as Time
import InternalAnim.Transition as Transition
import InternalAnim.Units as Units


{-| If this is a simple transition, then we can render it as a browser transition.

Otherwise, if it has sequences, then it needs to be rendered as a CSS animation.

Value animations don't care.

-}
hasSequence : Move value -> Bool
hasSequence (Pos _ _ seq) =
    not (List.isEmpty seq)


{-| -}
type Move value
    = Pos Transition.Transition value (List (Sequence value))


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


{-| -}
withTransition : Transition.Transition -> Move value -> Move value
withTransition trans (Pos _ value sequence) =
    Pos
        trans
        value
        sequence


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


{-| -}
to : value -> Move value
to v =
    Pos Transition.standard v []


{-| -}
toWith : Transition.Transition -> value -> Move value
toWith t v =
    Pos t v []


move : Transition.Transition -> value -> List (Sequence value) -> Move value
move =
    Pos


addSequence : Int -> Duration.Duration -> List (Step value) -> Move value -> Move value
addSequence n dur steps (Pos transition value seq) =
    Pos transition value (seq ++ [ Sequence n Time.zeroDuration dur steps ])


getSequenceDuration : Sequence value -> Duration.Duration
getSequenceDuration (Sequence i delay dur steps) =
    dur


withSequenceDelay : Duration.Duration -> Sequence value -> Sequence value
withSequenceDelay delay (Sequence i _ dur steps) =
    Sequence i delay dur steps


stepWith : Duration.Duration -> Transition.Transition -> value -> Step value
stepWith =
    Step


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


lerpFloat : Float -> Float -> Float -> Float
lerpFloat t one two =
    one + ((two - one) * t)


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


type alias Vector =
    { x : Float
    , y : Float
    , z : Float
    }


lerpVector : Float -> Vector -> Vector -> Vector
lerpVector t one two =
    { x =
        lerpFloat t one.x two.x
    , y =
        lerpFloat t one.y two.y
    , z =
        lerpFloat t one.z two.z
    }


at :
    Float
    -> Time.Absolute
    -> Time.Absolute
    -> Move Float
    -> State
    -> State
at progress startTime targetTime (Pos transition targetPosition dwell) startingState =
    let
        startPosition =
            Units.inPixels startingState.position
    in
    Transition.atX progress transition
        |> denormalize startTime
            targetTime
            startPosition
            targetPosition


{-|

    Go from 0-1:0-1

    To startPot-endPos,startTime-endTime

-}
denormalize :
    Time.Absolute
    -> Time.Absolute
    -> Float
    -> Float
    ->
        { position : Bezier.Point
        , velocity : Bezier.Point
        }
    -> State
denormalize startTime targetTime startPosition targetPosition state =
    { position =
        Units.pixels
            (toReal
                startPosition
                targetPosition
                state.position.y
            )
    , velocity =
        let
            scaled =
                state.velocity
                    |> scaleXYBy
                        { x =
                            Duration.inSeconds
                                (Time.duration startTime targetTime)
                        , y = targetPosition - startPosition
                        }
        in
        if scaled.x == 0 then
            Units.pixelsPerSecond 0

        else
            Units.pixelsPerSecond (scaled.y / scaled.x)
    }


scaleXYBy : { x : Float, y : Float } -> Bezier.Point -> Bezier.Point
scaleXYBy { x, y } point =
    { x = point.x * x, y = point.y * y }


{-| The opposite of `normalizeOver`.

I guess this is denormalization? Though i was always confused by that term :/

-}
toReal : Float -> Float -> Float -> Float
toReal start end t =
    start + (t * (end - start))


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



{- CSS KEYFRAMES -}


hash : Time.Absolute -> String -> Sequence value -> (value -> String) -> String
hash now name (Sequence n delay dur steps) toString =
    -- we need to encode the current time in the animations name so the browser doesn't cache anything
    -- IM LOOKIN AT YOU, CHROME
    name
        ++ String.fromInt (round <| Time.inMilliseconds now)
        ++ (if n == 1 then
                ""

            else
                "n" ++ String.fromInt n
           )
        ++ hashDuration "dl" delay
        ++ hashDuration "d" dur
        ++ stepHash steps toString ""


stepHash : List (Step value) -> (value -> String) -> String -> String
stepHash steps toString hashed =
    case steps of
        [] ->
            hashed

        (Step dur trans v) :: remain ->
            stepHash
                remain
                toString
                (hashed
                    -- ++ "--"
                    ++ hashDuration "d" dur
                    ++ Transition.hash trans
                    ++ "-"
                    ++ toString v
                )


hashDuration : String -> Duration.Duration -> String
hashDuration prefix dur =
    let
        seconds =
            Duration.inSeconds dur
    in
    if seconds == 0 then
        ""

    else
        prefix
            ++ String.fromInt
                (round (Duration.inSeconds dur))


type alias CssAnim =
    { hash : String
    , animation : String
    , keyframes : String
    , transition : String
    , props : List ( String, String )
    }


type AllowTransitions
    = AllowTransitions
    | DisallowTransitions


{-|

    - For the first curve of the first step of the first sequence
        -> we can render a `transition` and render a property equal to the end of the transition.
        -> this transition has it's own timing function and everything
        -> Create a new `Sequence` for the tail

    - We then render the rest of the sequences

-}
cssForSections :
    Time.Absolute
    -> AllowTransitions
    -> value
    -> String
    -> (Float -> value -> value -> String)
    -> (value -> String)
    -> (value -> String)
    -> List (Sequence value)
    -> CssAnim
    -> CssAnim
cssForSections now allowTransitions startPos name lerp toString toHashString sections anim =
    case sections of
        [] ->
            anim

        [ (Sequence 1 delay _ [ Step stepDur (Transition.Transition spline) v ]) as seq ] ->
            case allowTransitions of
                AllowTransitions ->
                    -- NOTE, we're not using the above `dur` because it's the total duration of the sequence
                    -- and therefore equal to stepDur in this case
                    { anim
                        | hash =
                            case anim.hash of
                                "" ->
                                    hash now name seq toHashString

                                _ ->
                                    anim.hash ++ hash now name seq toHashString
                        , transition =
                            case anim.transition of
                                "" ->
                                    renderTransition name delay stepDur spline

                                _ ->
                                    renderTransition name delay stepDur spline ++ ", " ++ anim.transition
                        , props =
                            ( name, toString v )
                                :: anim.props
                    }

                DisallowTransitions ->
                    toCssKeyframes
                        now
                        startPos
                        name
                        lerp
                        toString
                        toHashString
                        sections
                        anim

        _ ->
            toCssKeyframes
                now
                startPos
                name
                lerp
                toString
                toHashString
                sections
                anim


renderTransition : String -> Duration.Duration -> Duration.Duration -> Bezier.Spline -> String
renderTransition prop delay duration spline =
    prop
        ++ " "
        ++ Time.durationToString duration
        ++ " "
        ++ Bezier.toCss spline
        ++ " "
        ++ Time.durationToString delay


{-|

    1. renders a transition for the first step in the first sequence

-}
toCssKeyframes :
    Time.Absolute
    -> value
    -> String
    -> (Float -> value -> value -> String)
    -> (value -> String)
    -> (value -> String)
    -> List (Sequence value)
    -> CssAnim
    -> CssAnim
toCssKeyframes now startPos name lerp toString toHashString sections anim =
    case sections of
        [] ->
            anim

        seq :: remain ->
            toCssKeyframes
                now
                startPos
                name
                lerp
                toString
                toHashString
                remain
                (combine
                    (css now startPos name lerp toString toHashString seq)
                    anim
                )


css :
    Time.Absolute
    -> value
    -> String
    -> (Float -> value -> value -> String)
    -> (value -> String)
    -> (value -> String)
    -> Sequence value
    -> CssAnim
css now startPos name lerp toString toHashString seq =
    let
        animationName =
            hash now
                name
                seq
                toHashString

        n =
            case seq of
                Sequence i _ _ _ ->
                    if i == -1 then
                        "infinite"

                    else if i <= 0 then
                        "1"

                    else
                        String.fromInt i

        durationStr =
            case seq of
                Sequence _ _ dur _ ->
                    Time.durationToString dur

        delayStr =
            case seq of
                Sequence _ delay _ _ ->
                    Time.durationToString delay
    in
    { hash = animationName
    , transition = ""
    , animation =
        durationStr
            -- we specify an easing function here because it we have to
            -- , but it is overridden by the one in keyframes
            ++ " linear "
            ++ delayStr
            ++ " "
            ++ n
            ++ " normal forwards running "
            ++ animationName
    , keyframes =
        ("@keyframes " ++ animationName ++ " {\n")
            ++ keyframes name lerp startPos seq ""
            ++ "\n}"
    , props = initialProps name toString startPos seq
    }


{-| If the first step is a `set`, then we set those properties immediately
-}
initialProps : String -> (value -> String) -> value -> Sequence value -> List ( String, String )
initialProps name toString startPos (Sequence _ _ _ steps) =
    case steps of
        [] ->
            []

        (Step dur _ val) :: _ ->
            if Duration.isZero dur then
                [ ( name, toString val ) ]

            else
                []


keyframes : String -> (Float -> value -> value -> String) -> value -> Sequence value -> String -> String
keyframes name lerp startPos (Sequence _ _ dur steps) rendered =
    keyframeHelper name
        lerp
        startPos
        dur
        zeroDuration
        steps
        rendered


keyframeHelper :
    String
    -> (Float -> value -> value -> String)
    -> value
    -> Time.Duration
    -> Time.Duration
    -> List (Step value)
    -> String
    -> String
keyframeHelper name lerp startPos sequenceDuration currentDur steps rendered =
    case steps of
        [] ->
            rendered

        (Step dur transition val) :: remaining ->
            let
                nextCurrent =
                    Time.expand currentDur dur

                startPercent =
                    Time.progressWithin currentDur sequenceDuration * 100

                endPercent =
                    Time.progressWithin nextCurrent sequenceDuration * 100

                frames =
                    Transition.keyframes
                        (\t ->
                            lerp t startPos val
                        )
                        startPercent
                        endPercent
                        transition
            in
            keyframeHelper
                name
                lerp
                val
                sequenceDuration
                nextCurrent
                remaining
                (rendered ++ frames)


combine : CssAnim -> CssAnim -> CssAnim
combine one two =
    if String.isEmpty one.hash && List.isEmpty one.props then
        two

    else if String.isEmpty two.hash && List.isEmpty two.props then
        one

    else
        { hash = one.hash ++ two.hash
        , animation =
            case one.animation of
                "" ->
                    two.animation

                _ ->
                    case two.animation of
                        "" ->
                            two.animation

                        _ ->
                            two.animation ++ ", " ++ one.animation
        , transition =
            case one.transition of
                "" ->
                    two.transition

                _ ->
                    case two.transition of
                        "" ->
                            two.transition

                        _ ->
                            two.transition ++ ", " ++ one.transition
        , keyframes =
            case one.keyframes of
                "" ->
                    two.keyframes

                _ ->
                    case two.keyframes of
                        "" ->
                            two.keyframes

                        _ ->
                            two.keyframes ++ "\n" ++ one.keyframes
        , props = one.props ++ two.props
        }
