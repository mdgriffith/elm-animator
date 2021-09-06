module Internal.Move exposing
    ( Move(..), to, toWith
    , State, init
    , color
    , Sequence
    , Step, step, stepWith, set
    , sequences
    , css, addSequence, cssForSections
    , withWobble, withDelay, withVelocities, withBezier
    , at, atX, transitionTo
    , denormalize, normalizeOver, toReal
    , floatToString, initialSequenceVelocity
    )

{-|

@docs Move, to, toWith

@docs State, init

@docs color

@docs Sequence
@docs Step, step, stepWith, set

@docs sequences, goto

@docs css, addSequence, cssForSections

@docs withWobble, withDelay, withVelocities, withBezier

@docs at, atX, transitionTo

@docs denormalize, normalizeOver, toReal

-}

import Color
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


init : Move Float -> State
init movement =
    { position =
        case movement of
            Pos _ x _ ->
                Pixels.pixels x
    , velocity = Pixels.pixelsPerSecond 0
    }


withDelay : Duration.Duration -> Move value -> Move value
withDelay dur (Pos trans value sequence) =
    Debug.todo "What do we do here?"


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


withWobble : Float -> Move value -> Move value
withWobble wob (Pos trans value sequence) =
    Pos
        (Transition.wobble wob)
        value
        sequence


withBezier : Float -> Float -> Float -> Float -> Move value -> Move value
withBezier one two three four (Pos trans value sequence) =
    Pos
        (Transition.bezier one two three four)
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

-}
type
    Sequence value
    --         repeat, delay,        duration
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


addSequence : Int -> Duration.Duration -> List (Step value) -> Move value -> Move value
addSequence n dur steps (Pos transition value seq) =
    Pos transition value (seq ++ [ Sequence n Time.zeroDuration dur steps ])


getSequenceDuration : Sequence value -> Duration.Duration
getSequenceDuration (Sequence i delay dur steps) =
    dur


withSequenceDelay : Duration.Duration -> Sequence value -> Sequence value
withSequenceDelay delay (Sequence i _ dur steps) =
    Sequence i delay dur steps


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


color : Float -> Color.Color -> Color.Color -> Color.Color
color progress from target =
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


atX :
    Float
    -> Move Float
    ->
        { position : Bezier.Point
        , velocity : Bezier.Point
        }
atX progress (Pos trans value dwell) =
    Transition.atX2 progress trans


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
            Pixels.inPixels startingState.position
    in
    Transition.atX2 progress transition
        |> denormalize startTime
            targetTime
            startPosition
            targetPosition


{-| This is the same as `at`, but with velocity transitions built in.
-}
transitionTo :
    Float
    -> Time.Absolute
    -> Time.Absolute
    -> Move Float
    -> State
    -> State
transitionTo progress startTime targetTime (Pos trans targetPosition dwell) startingState =
    let
        startPosition =
            Pixels.inPixels startingState.position

        introVelocity =
            normalizeVelocity
                startTime
                targetTime
                startPosition
                targetPosition
                startingState.velocity

        -- exitVelocity =
        -- If we do any transition smoothing
        -- we'll need to normalize this velocity too
        --Estimation.velocityAtTarget lookupState target future
        transition =
            if introVelocity == 0 then
                trans

            else
                Transition.withVelocities introVelocity 0 trans
    in
    Transition.atX2 progress transition
        |> denormalize startTime
            targetTime
            startPosition
            targetPosition


normalizeVelocity :
    Time.Absolute
    -> Time.Absolute
    -> Float
    -> Float
    -> Units.PixelsPerSecond
    -> Float
normalizeVelocity startTime targetTime startPosition targetPosition velocity =
    let
        pixelsPerSecond =
            Pixels.inPixelsPerSecond velocity
    in
    if pixelsPerSecond == 0 then
        0

    else
        (pixelsPerSecond * Duration.inSeconds (Time.duration startTime targetTime))
            / (targetPosition - startPosition)


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
        Pixels.pixels
            (toReal
                startPosition
                targetPosition
                state.position.y
            )
    , velocity =
        let
            scaled =
                state.velocity
                    |> Bezier.scaleXYBy
                        { x =
                            Duration.inSeconds
                                (Time.duration startTime targetTime)
                        , y = targetPosition - startPosition
                        }
        in
        if scaled.x == 0 then
            Pixels.pixelsPerSecond 0

        else
            Pixels.pixelsPerSecond (scaled.y / scaled.x)
    }


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
                    splitTime =
                        Time.progress startTime targetTime now

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

            Pos trans value [ Sequence 1 delay dur steps ] ->
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
                let
                    transitionDuration =
                        Time.duration startTime targetTime
                in
                if Time.thisAfterThat now targetTime then
                    takeAfterSequenceList durationToNow dwell

                else
                    let
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

        ((Sequence n delay duration steps) as top) :: remain ->
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
    Quantity.Quantity Float Duration.Seconds
    -> Sequence value
    -> List (Sequence value)
    -> Bool
afterSequenceList durationTillNow seq remaining =
    if afterSequence durationTillNow seq then
        case remaining of
            [] ->
                True

            ((Sequence n delay duration steps) as top) :: rest ->
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


zeroDuration : Duration.Duration
zeroDuration =
    Duration.milliseconds 0



{- CSS KEYFRAMES -}


initialSequenceVelocity : Sequence value -> PixelsPerSecond
initialSequenceVelocity seq =
    case seq of
        Sequence 0 _ _ _ ->
            zeroVelocity

        Sequence _ _ _ [] ->
            zeroVelocity

        Sequence n _ _ ((Step dur trans _) :: _) ->
            Transition.initialVelocity trans
                |> (*) 1000
                |> Pixels.pixelsPerSecond


hash : String -> Sequence value -> (value -> String) -> String
hash name (Sequence n delay dur steps) toString =
    name ++ String.fromInt n ++ stepHash steps toString ""


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
                    ++ "--"
                    ++ hashDuration dur
                    ++ "-"
                    ++ Transition.hash trans
                    ++ "-"
                    ++ toString v
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
lastPosOr x (Sequence _ _ _ steps) =
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


type alias CssAnim =
    { hash : String
    , animation : String
    , keyframes : String
    , props : List ( String, String )
    }


{-|

    - For the first curve of the first step of the first sequence
        -> we can render a `transition` and render a property equal to the end of the transition.
        -> this transition has it's own timing function and everything
        -> Create a new `Sequence` for the tail

    - We then render the rest of the sequences

-}
cssForSections :
    Time.Absolute
    -> value
    -> String
    -> (value -> String)
    -> (value -> String)
    -> List (Sequence value)
    -> CssAnim
    -> CssAnim
cssForSections now startPos name toString toHashString sections anim =
    case sections of
        [] ->
            anim

        (Sequence 1 delay dur [ Step stepDur (Transition.Transition spline) v ]) :: rest ->
            -- most common situation
            toCssKeyframes
                now
                startPos
                name
                toString
                toHashString
                rest
                { anim
                    | props =
                        renderTransition name delay stepDur spline
                            :: ( name, toString v )
                            :: anim.props
                }

        (Sequence 1 delay dur [ Step stepDur (Transition.Wobble wob) v ]) :: rest ->
            Debug.todo "figure out where to split the wob into a trail"

        (Sequence i delay dur (((Step stepDur trans v) :: remainingSteps) as allSteps)) :: rest ->
            let
                subsequentDuration =
                    dur |> Time.reduceDurationBy stepDur
            in
            cssForSections now
                startPos
                name
                toString
                toHashString
                (Sequence 1 delay stepDur [ Step stepDur trans v ]
                    :: Sequence 1 (Time.expand delay stepDur) subsequentDuration remainingSteps
                    :: Sequence (i - 1)
                        (Time.expand delay dur)
                        dur
                        allSteps
                    :: rest
                )
                anim

        (Sequence i delay dur steps) :: rest ->
            -- there are no steps in this sequence
            anim


renderTransition : String -> Duration.Duration -> Duration.Duration -> Bezier.Spline -> ( String, String )
renderTransition prop delay duration spline =
    ( "transition"
    , prop
        ++ " "
        ++ Time.durationToString duration
        ++ " "
        ++ Bezier.cssTimingString spline
        ++ " "
        ++ Time.durationToString delay
    )


{-|

    1. renders a transition for the first step in the first sequence

-}
toCssKeyframes :
    Time.Absolute
    -> value
    -> String
    -> (value -> String)
    -> (value -> String)
    -> List (Sequence value)
    -> CssAnim
    -> CssAnim
toCssKeyframes now startPos name toString toHashString sections anim =
    case sections of
        [] ->
            anim

        seq :: remain ->
            toCssKeyframes
                now
                startPos
                name
                toString
                toHashString
                remain
                (combine
                    (css now startPos name toString toHashString seq)
                    anim
                )


css :
    Time.Absolute
    -> value
    -> String
    -> (value -> String)
    -> (value -> String)
    -> Sequence value
    ->
        { hash : String
        , animation : String
        , keyframes : String
        , props : List a
        }
css now startPos name toString toHashString seq =
    let
        animationName =
            -- we need to encode the current time in the animations name so the browser doesn't cache anything
            -- IM LOOKIN AT YOU, CHROME
            hash (name ++ String.fromInt (round <| Time.inMilliseconds now))
                seq
                toHashString

        n =
            case seq of
                Sequence i _ _ _ ->
                    if i <= 0 then
                        "1"

                    else if isInfinite (toFloat i) then
                        "infinite"

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


keyframes : String -> value -> (value -> String) -> Sequence value -> String -> String
keyframes name startPos toString (Sequence _ _ dur steps) rendered =
    keyframeHelper name
        startPos
        (\v -> name ++ ":" ++ toString v)
        dur
        zeroDuration
        steps
        rendered


keyframeHelper :
    String
    -> value
    -> (value -> String)
    -> Time.Duration
    -> Time.Duration
    -> List (Step value)
    -> String
    -> String
keyframeHelper name startPos toString sequenceDuration currentDur steps rendered =
    case steps of
        [] ->
            rendered

        (Step dur transition val) :: [] ->
            let
                last =
                    "100% { " ++ (toString val ++ ";}")

                domain =
                    { start =
                        startPos
                    , end =
                        val
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
                        toString
                        transition
            in
            rendered ++ frames ++ last

        (Step dur transition val) :: remaining ->
            let
                domain =
                    { start = startPos
                    , end = val
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


emptyAnim : CssAnim
emptyAnim =
    { hash = ""
    , animation = ""
    , keyframes = ""
    , props = []
    }


combine : CssAnim -> CssAnim -> CssAnim
combine one two =
    if String.isEmpty one.hash && List.isEmpty one.props then
        two

    else if String.isEmpty two.hash && List.isEmpty two.props then
        one

    else
        { hash = one.hash ++ two.hash
        , animation = two.animation ++ ", " ++ one.animation
        , keyframes = one.keyframes ++ "\n" ++ two.keyframes
        , props = one.props ++ two.props
        }
