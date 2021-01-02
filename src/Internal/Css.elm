module Internal.Css exposing (..)

{-| -}

import Dict exposing (Dict)
import Duration
import Internal.Bezier as Bezier
import Internal.Interpolate as Interpolate
import Internal.Time as Time
import Internal.Timeline as Timeline
import Pixels
import Set exposing (Set)


type Keyframe value
    = Keyframe
        { value : value
        , timestamp : Float
        }


type KeyframeSet value
    = KeyframeSet
        { duration : Int
        , delay : Int
        , repeat : Float
        , keyframes : List (Keyframe value)
        }
    | Transition
        { duration : Int
        , delay : Int
        , target : Keyframe value
        }


type Sequence value
    = Sequence
        { duration : Int
        , keyframes : List (KeyframeSet value)
        , dwell :
            Maybe
                { period : Timeline.Period
                , keyframes : List (Keyframe value)
                }
        }


emptySequence : Sequence value
emptySequence =
    Sequence
        { duration = 0
        , keyframes = []
        , dwell = Nothing
        }


{-| From this we need to render css @keyframes

Example keyframes:

    p {
        animation-duration: 3s;
        animation-name: one;
        animation-delay: 300ms;
        animation-iteration-count: infinite | 5;
    }

    @keyframes transitionOne {
        0% {
            -- we can define the timing function which says how to interpolate from this keyframe to the next
            animation-timing-function: cubic-bezier(0.19, 1, 0.22, 1);
            transform: translate(0px, 0px)
        }
        100% {
            transform: translate(1000px, 1000px)

        }
    }

-}
css :
    String
    -> (Float -> String)
    -> (state -> Interpolate.Movement)
    -> Timeline.Timeline state
    -> CssAnim
css name renderValue lookup timeline =
    let
        now =
            Timeline.getCurrentTime timeline
    in
    case Timeline.foldp lookup (toCss now name renderValue lookup) timeline of
        result ->
            result
                |> finalize name renderValue now
                |> combine result.css


finalize :
    String
    -> (Float -> String)
    -> Time.Absolute
    ->
        { stack
            | stackStart : Time.Absolute
            , stackEnd : Time.Absolute
            , stack : List Interpolate.Checkpoint
        }
    -> CssAnim
finalize name renderValue now stack =
    let
        animationName =
            name ++ "-"

        durationStr =
            String.fromFloat (Duration.inMilliseconds (Time.duration stack.stackStart stack.stackEnd)) ++ "ms"

        delay =
            Time.duration now stack.stackStart
                |> Duration.inMilliseconds
                |> String.fromFloat
                |> (\s -> s ++ "ms")

        -- @keyframes duration | easing-function | delay |
        --      iteration-count | direction | fill-mode | play-state | name */
        -- animation: 3s ease-in 1s 2 reverse both paused slidein;
        animation =
            "animation: "
                ++ (durationStr ++ " ")
                -- we specify an easing function here because it we have to
                -- , but it is overridden by the one in keyframes
                ++ "linear "
                ++ delay
                ++ " 1 normal forward running "
                ++ animationName
                ++ ";"

        keyframes =
            "@keyframes {"
                ++ checkpointKeyframes name
                    renderValue
                    stack.stack
                    ""
                ++ "}"
    in
    { hash = animationName
    , animation = animation
    , keyframes = keyframes
    }


finalizeTransition :
    String
    -> (Float -> String)
    -> Time.Absolute
    -> List Bezier.Spline
    -> Time.Absolute
    -> Time.Duration
    -> CssAnim
finalizeTransition name renderValue now splines start totalDuration =
    { hash = ""

    -- use single prop encoding:
    -- https://developer.mozilla.org/en-US/docs/Web/CSS/animation
    , animation = ""
    , keyframes = ""
    }


{-| Every sequential event is visited.
-> add current event to most recent KeyframeSet

If we're interrupted vefore visiting a state, then lerp is called.
-> lerp always creates a totally new `KeyframeSet`

-}
toCss :
    Time.Absolute
    -> String
    -> (Float -> String)
    -> (state -> Interpolate.Movement)
    ->
        Timeline.Interp state
            Interpolate.Movement
            { css : CssAnim
            , stackStart : Time.Absolute
            , stackEnd : Time.Absolute
            , stack : List Interpolate.Checkpoint
            , state : Interpolate.State
            }
toCss now name renderValue toMotion =
    { start =
        \motion ->
            { css =
                { hash = ""
                , animation = ""
                , keyframes = ""
                }
            , stackStart = now
            , stackEnd = now
            , stack = []
            , state = Interpolate.moving.start motion
            }
    , adjustor =
        \_ ->
            Timeline.linearDefault
    , dwellPeriod =
        \_ ->
            Nothing
    , visit =
        \lookup target targetTime maybeLookAhead data ->
            let
                state =
                    Interpolate.moving.visit
                        lookup
                        target
                        targetTime
                        maybeLookAhead
                        data.state
            in
            -- Add keyframe to current stack
            -- if there is a lookahead, add timing fn for that lookahead
            -- otherwise render the dwelling behavior as a separate anim
            case maybeLookAhead of
                Nothing ->
                    -- capture
                    --    - target pos
                    --    - target time
                    --    - no timing
                    --
                    -- if dwell
                    --     - finalize stack
                    --     - render and finalize dwell
                    let
                        newStack =
                            { time = Time.inMilliseconds targetTime
                            , value = Pixels.inPixels state.position
                            , timing = Interpolate.Linear
                            }
                                :: data.stack

                        final =
                            { css = data.css
                            , stackStart = data.stackStart
                            , stackEnd = data.stackEnd
                            , stack =
                                newStack
                            , state = state
                            }
                    in
                    addDwell lookup
                        name
                        renderValue
                        target
                        targetTime
                        now
                        state
                        { css =
                            final
                                |> finalize name renderValue now
                                |> combine data.css
                        , stackStart = targetTime
                        , stackEnd = targetTime
                        , stack = []
                        , state = state
                        }

                Just lookAhead ->
                    -- capture
                    --    - target pos
                    --    - target time
                    --    - capture timing to lookahead
                    { css =
                        data.css
                    , stackStart = data.stackStart
                    , stackEnd = data.stackEnd
                    , stack =
                        { time = Time.inMilliseconds targetTime
                        , value = Pixels.inPixels state.position
                        , timing = Interpolate.Linear
                        }
                            :: data.stack
                    , state = state
                    }
    , lerp =
        \prevEndTime prev target targetTime now_IGNORE maybeLookAhead data ->
            -- finalize current stack
            -- create and finalizeTransition stack for the interruption
            -- (but use the special transition finalizer which embeds timing outside of keyframes)
            let
                transitionSplines =
                    Interpolate.lerpSplines
                        prevEndTime
                        prev
                        target
                        targetTime
                        maybeLookAhead
                        data.state
            in
            { css =
                finalize name renderValue now data
                    |> combine data.css
                    |> combine
                        (finalize name
                            renderValue
                            now
                            { stackStart = Time.millis prevEndTime
                            , stackEnd = Time.millis targetTime
                            , stack =
                                normalizeToCheckpoints
                                    (Time.duration
                                        (Time.millis prevEndTime)
                                        (Time.millis targetTime)
                                    )
                                    transitionSplines
                            }
                        )
            , stackStart = Time.millis targetTime
            , stackEnd = Time.millis targetTime
            , stack = []
            , state =
                Interpolate.moving.lerp
                    prevEndTime
                    prev
                    target
                    targetTime
                    now_IGNORE
                    maybeLookAhead
                    data.state
            }
    }


normalizeToCheckpoints : Time.Duration -> List Bezier.Spline -> List Interpolate.Checkpoint
normalizeToCheckpoints duration splines =
    List.map (toCheckpoint duration) splines


toCheckpoint : Time.Duration -> Bezier.Spline -> Interpolate.Checkpoint
toCheckpoint duration ((Bezier.Spline c0 c1 c2 c3) as spline) =
    { value = c3.y
    , timing = Interpolate.Bezier (Bezier.normalize spline)
    , time = c3.x
    }


addDwell :
    (state -> Interpolate.Movement)
    -> String
    -> (Float -> String)
    -> Timeline.Occurring state
    -> Time.Absolute
    -> Time.Absolute
    -> Interpolate.State
    ->
        { css : CssAnim
        , stackStart : Time.Absolute
        , stackEnd : Time.Absolute
        , stack : List Interpolate.Checkpoint
        , state : Interpolate.State
        }
    ->
        { css : CssAnim
        , stackStart : Time.Absolute
        , stackEnd : Time.Absolute
        , stack : List Interpolate.Checkpoint
        , state : Interpolate.State
        }
addDwell lookup name renderValue target startTime now state details =
    case lookup (Timeline.getEvent target) of
        Interpolate.Osc personality startPos period checkpoints ->
            let
                animationName =
                    name ++ "-dwell"

                durationStr =
                    String.fromFloat (Duration.inMilliseconds duration) ++ "ms"

                duration =
                    case period of
                        Timeline.Loop dur ->
                            dur

                        Timeline.Repeat n dur ->
                            dur

                delay =
                    Time.duration now startTime
                        |> Duration.inMilliseconds
                        |> String.fromFloat
                        |> (\s -> s ++ "ms")

                iterationCount =
                    case period of
                        Timeline.Loop dur ->
                            "infinite"

                        Timeline.Repeat n dur ->
                            String.fromInt n

                -- @keyframes duration | easing-function | delay |
                --      iteration-count | direction | fill-mode | play-state | name */
                -- animation: 3s ease-in 1s 2 reverse both paused slidein;
                animation =
                    "animation: "
                        ++ (durationStr ++ " ")
                        -- we specify an easing function here because it we have to
                        -- , but it is overridden by the one in keyframes
                        ++ "linear "
                        ++ (delay ++ " ")
                        ++ iterationCount
                        ++ " normal forward running "
                        ++ animationName
                        ++ ";"

                keyframes =
                    "@keyframes {"
                        ++ checkpointKeyframes name
                            renderValue
                            checkpoints
                            ""
                        ++ "}"

                new =
                    { hash = animationName
                    , animation = animation
                    , keyframes = keyframes
                    }
            in
            { details
                | css =
                    combine details.css new
            }

        Interpolate.Pos _ _ ->
            details


checkpointKeyframes : String -> (Float -> String) -> List Interpolate.Checkpoint -> String -> String
checkpointKeyframes name renderValue checkpoints rendered =
    case checkpoints of
        [] ->
            rendered

        top :: remaining ->
            let
                percentage =
                    String.fromFloat (top.time * 100) ++ "%"

                frame =
                    percentage
                        ++ "{"
                        ++ (name ++ ":" ++ renderValue top.value ++ ";")
                        ++ ("animation-timing-function:" ++ renderTiming top.timing ++ ";")
                        ++ "}"
            in
            checkpointKeyframes name
                renderValue
                remaining
                (rendered ++ frame)


renderTiming : Interpolate.Timing -> String
renderTiming timing =
    case timing of
        Interpolate.Linear ->
            "linear"

        Interpolate.Bezier (Bezier.Spline c0 c1 c2 c3) ->
            -- the spline passed here needs to be normalized over 0-1
            -- and then we only need to pass the two control points to the css animation
            "cubic-bezier("
                ++ (String.fromFloat c1.x ++ ", ")
                ++ (String.fromFloat c1.y ++ ", ")
                ++ (String.fromFloat c2.x ++ ", ")
                ++ String.fromFloat c2.y
                ++ ")"


{-| -}
type alias CssAnim =
    { hash : String

    -- use single prop encoding:
    -- https://developer.mozilla.org/en-US/docs/Web/CSS/animation
    , animation : String
    , keyframes : String
    }


type alias Key =
    Int


compoundSequence :
    (state -> List ( Key, Interpolate.Movement ))
    ->
        Timeline.Interp state
            Interpolate.Movement
            { sequence : Sequence (Dict Key Interpolate.Checkpoint)
            , transformConflict : Bool
            , keys : Set Key
            }
compoundSequence toMotion =
    Debug.todo ""


{-| render keyframes to string

    p {
        animation-duration: 3s;
        animation-name: one;
        animation-delay: 300ms;
        animation-iteration-count: infinite | 5;
    }

    @keyframes transitionOne {
        0% {
            -- we can define the timing function which says how to interpolate from this keyframe to the next
            animation-timing-function: cubic-bezier(0.19, 1, 0.22, 1);
            transform: translate(0px, 0px)
        }
        100% {
            transform: translate(1000px, 1000px)

        }
    }

Transitions:

When doing transitions, we can't add a `timing-fn` to a keyframe with no values, which would have been perfect.

Instead, for each transition, we need to compose a full animation

Example: <https://codepen.io/mechanical-elephant/pen/MWjEXzq>

    @keyframes normal {
        from {
            transform: translateX(0px);
        }
        to {
            transform: translateX(200px);
        }
    }


    @keyframes transition {
        to {
            transform: translateX(1200px);
        }
    }

    /* The element to apply the animation to */
    .item {
        width: 100px;
        height: 100px;
        background-color:red;
        transform: translateX(0px);
        animation-timing-function: linear, cubic-bezier(0.1, -0.6, 0.2, 0);
        animation-name: normal, transition;
        animation-delay: 0ms, 4s;
        animation-duration: 8s, 4s;
    }

Each property, and each "set" will render as a separate keyframe statement

-}
encode :
    (frame
     ->
        -- this is timing to get to this state
        -- for transitions, this turns out to be what we want
        -- for normal keyframes, we have to shift it back one step.
        { timing : Interpolate.Timing
        , name : String
        , value : String
        }
    )
    -> Sequence frame
    -> CssAnim
encode fn (Sequence seq) =
    { hash = ""
    , animation = ""
    , keyframes = ""
    }


{-| A less efficient version of the above that encodes every frame as a keyframe.

returned frames will be distributed evenly.

-}
encodeEveryFrame :
    (frame
     ->
        -- this is timing to get to this state
        -- for transitions, this turns out to be what we want
        -- for normal keyframes, we have to shift it back one step.
        List
            { name : String
            , value : String
            }
    )
    -> Sequence frame
    -> CssAnim
encodeEveryFrame fn (Sequence seq) =
    { hash = ""
    , animation = ""
    , keyframes = ""
    }


combine : CssAnim -> CssAnim -> CssAnim
combine one two =
    { hash = one.hash ++ two.hash
    , animation = one.animation ++ ", " ++ two.animation
    , keyframes = one.keyframes ++ "\n" ++ two.keyframes
    }
