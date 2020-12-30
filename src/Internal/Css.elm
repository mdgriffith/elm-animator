module Internal.Css exposing (..)

{-| -}

import Dict exposing (Dict)
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


type alias Bezier =
    { one : Point
    , oneControl : Point
    , two : Point
    , twoControl : Point
    }


type alias Point =
    { x : Float, y : Float }


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
    case Timeline.foldp lookup (toCss (Timeline.getCurrentTime timeline) name lookup) timeline of
        result ->
            result
                |> finalize name renderValue
                |> combine result.css


type alias Frame =
    { timestamp : Float
    , value : Float
    , timing : String
    }


finalize :
    String
    -> (Float -> String)
    ->
        { stack
            | stackDuration : Float
            , stackStart : Time.Absolute
            , stack : List Frame
        }
    -> CssAnim
finalize name renderValue stack =
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
    -> (state -> Interpolate.Movement)
    ->
        Timeline.Interp state
            Interpolate.Movement
            { css : CssAnim
            , stackDuration : Float
            , stackStart : Time.Absolute
            , stack : List Frame
            , state : Interpolate.State
            }
toCss now name toMotion =
    { start =
        \motion ->
            { css =
                { hash = ""
                , animation = ""
                , keyframes = ""
                }
            , stackDuration = 0
            , stackStart = now
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
                    { css =
                        data.css
                    , stackDuration = data.stackDuration
                    , stackStart = data.stackStart
                    , stack =
                        { timestamp = Time.inMilliseconds targetTime
                        , value = Pixels.inPixels state.position
                        , timing = ""
                        }
                            :: data.stack
                    , state = state
                    }

                Just lookAhead ->
                    -- capture
                    --    - target pos
                    --    - target time
                    --    - capture timing to lookahead
                    { css =
                        data.css
                    , stackDuration = 0
                    , stackStart = data.stackStart
                    , stack =
                        { timestamp = Time.inMilliseconds targetTime
                        , value = Pixels.inPixels state.position
                        , timing = ""
                        }
                            :: data.stack
                    , state = state
                    }
    , lerp =
        \prevEndTime prev target targetTime now_IGNORE maybeLookAhead data ->
            -- finalize current stack
            -- create and finalizeTransition stack for the interruption
            -- (but use the special transition finalizer which embeds timing outside of keyframes)
            --
            { css =
                data.css
            , stackDuration = 0
            , stackStart = data.stackStart
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
