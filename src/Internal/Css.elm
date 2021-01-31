module Internal.Css exposing (..)

{-| -}

import Color
import Duration
import Html.Attributes exposing (id)
import Internal.Bezier as Bezier
import Internal.Css.Props
import Internal.Interpolate as Interpolate
import Internal.Time as Time
import Internal.Timeline as Timeline
import Pixels


{-| An id representing a prop type.

Like
1 ->
background-color

-}
type alias Id =
    Int


{-| Generally when doing transitions, we want to define a list of properties instead of a single one.

    Open ->
        [ x 200
        , opacity 1
        ]

    Closed ->
        [ x 0
        , opacity 0
        ]

These properties sometimes need to be grouped, as with transforms and colors.

Sometimes this batching is present in the elm-animator API (i.e. colors)

    - Though an alpha channel may be rendered separately!

Sometimes not (transforms, CSS requires them).

All properties will need an inherent default in case they are missing.

-}
type Prop
    = -- binary id for comparisons
      Prop Id Interpolate.Movement


{-| This is mainly to help rendering a transform.

If more than one of these properties is present for any state,
then these properties are overlapping.

This is because if there is both
rotate and translateX
or even
translateX and translateY

Then we can't use a single timing operation to describe them.

So, we'll need to render each frame.

Other properties, like background-color and color can be rendered independently by simply being in separate animations.

-}
overlapping :
    List Prop
    -> (state -> List Prop)
    -> Timeline.Timeline state
    -> Bool
overlapping only lookup timeline =
    False


type Property
    = --     name   suffix
      Color Id Color.Color
      -- This can be X, Y, Rotate, or Scale
    | Transform Id Interpolate.Movement


{-| -}
scan :
    (state -> List Prop)
    -> Timeline.Timeline state
    -> List Prop
scan lookup timeline =
    [ Prop Internal.Css.Props.ids.opacity (Interpolate.Pos Interpolate.standardDefault 0)

    ]


cssFromProps : Timeline.Timeline state -> (state -> List Prop) -> CssAnim
cssFromProps timeline lookup =
    let
        present =
            scan lookup timeline

        renderedProps =
            propsToCurves present lookup timeline
                |> Debug.log "rendered"
    in
    renderCss ( (Timeline.getCurrentTime timeline)) renderers renderedProps


{-| RenderdProp's are required to be ordered!
-}
type alias Renderer =
    Time.Absolute -> List RenderedProp -> Maybe ( CssAnim, List RenderedProp )


renderers : List Renderer
renderers =
    [ transform
    , colors
    , scalars
    ]


colors : Renderer
colors now renderedProps =
    -- ()
    -- Debug.todo ""
    Nothing


transform : Renderer
transform now renderedProps =
    case renderedProps of
        [] ->
            Nothing

        top :: remain ->
            let
                firstTransform =
                    isTransform top

                manyTransforms =
                    case remain of
                        penultimate :: _ ->
                            isTransform penultimate

                        _ ->
                            False
            in
            if manyTransforms then
                -- TODO: we can make this more nuanced once we have something working.
                Just (transformHelperExact renderedProps Internal.Css.Props.firstTransform)

            else if firstTransform then
                Just (transformHelper renderedProps Internal.Css.Props.firstTransform)

            else
                Nothing


isTransform : RenderedProp -> Bool
isTransform (RenderedProp prop) =
    Internal.Css.Props.isTransformId prop.id


{-| Render the exact points with a linear timing function. instead of relying on a cubic-bezier.
-}
transformHelperExact : List RenderedProp -> Id -> ( CssAnim, List RenderedProp )
transformHelperExact props expecting =
    case props of
        [] ->
            ( emptyAnim, [] )

        top :: remain ->
            ( emptyAnim
            , []
            )


{-| Transforms are weird because we can only render one timing-fn.

However, transforms are only in direct conflict some of the times.

The common behaviors are:

    translate -> 2 properties, each rendered with standardDefault personality
    scale -> 1 property, as standardDefault
    rotation -> usually *not* transitioned between concrete values,
        but could be rotating at a certain speed. when the state is at rest

transformHelper will render

-}
transformHelper : List RenderedProp -> Id -> ( CssAnim, List RenderedProp )
transformHelper props expecting =
    case props of
        [] ->
            ( emptyAnim, [] )

        top :: remain ->
            ( emptyAnim
            , []
            )


scalars : Renderer
scalars now renderedProps =
    case renderedProps of
        [] ->
            Nothing

        top :: remain ->
            Just (scalarHelper now renderedProps emptyAnim)


scalarHelper : Time.Absolute -> List RenderedProp -> CssAnim -> ( CssAnim, List RenderedProp )
scalarHelper now renderedProps anim =
    case renderedProps of
        [] ->
            ( anim, [] )

        top :: remain ->
            scalarHelper now
                remain
                (propToCss now top
                    |> combine anim
                )


propToCss : Time.Absolute -> RenderedProp -> CssAnim
propToCss now (RenderedProp details) =
    propToCssHelper now details.id details.sections emptyAnim


propToCssHelper : Time.Absolute -> Internal.Css.Props.Id -> List Section -> CssAnim -> CssAnim
propToCssHelper now id sections anim =
    case sections of
        [] ->
            anim

        top :: remain ->
            propToCssHelper now
                id
                remain
                (combine
                    anim
                    (sectionCss now id top)
                )


sectionCss : Time.Absolute -> Id -> Section -> CssAnim
sectionCss now id (Section section) =
    case section.splines of
        [] ->
            emptyAnim
        
        top :: remain ->
            if Bezier.afterLastX (Time.inMilliseconds now) top  then
                emptyAnim

            else
                let
                    name =
                        Internal.Css.Props.name id

                    toStr =
                        Internal.Css.Props.toStr id

                    animationName =
                        name
                            ++ "-"
                            ++ splineListHash section.splines ""

                    duration =
                        case section.period of
                            Timeline.Loop dur ->
                                dur

                            Timeline.Repeat _ dur ->
                                dur

                    durationStr =
                        String.fromFloat
                            (Duration.inMilliseconds duration)
                            ++ "ms"

                    delay =
                        Time.duration now section.start
                            |> Duration.inMilliseconds
                            |> String.fromFloat
                            |> (\s -> s ++ "ms")

                    n =
                        case section.period of
                            Timeline.Loop _ ->
                                infinite

                            Timeline.Repeat count _ ->
                                String.fromInt count

                    -- @keyframes duration | easing-function | delay |
                    --      iteration-count | direction | fill-mode | play-state | name */
                    -- animation: 3s ease-in 1s 2 reverse both paused slidein;
                    animation =
                        (durationStr ++ " ")
                            -- we specify an easing function here because it we have to
                            -- , but it is overridden by the one in keyframes
                            ++ "linear "
                            ++ delay
                            ++ " "
                            ++ n
                            ++ " normal forward running "
                            ++ animationName

                    keyframes =
                        ("@keyframes " ++ animationName ++ " {\n")
                            ++ sectionKeyFrames
                                section.start
                                now
                                (Time.advanceBy duration section.start)
                                name
                                toStr
                                section.splines
                                ""
                            ++ "\n}"
                in
                { hash = animationName
                , animation = animation
                , keyframes = keyframes
                }


{-| Reminder that `animation-timing-function` defines the timing function between the keyframe it's attached to and the next one.
-}
sectionKeyFrames : Time.Absolute -> Time.Absolute -> Time.Absolute -> String -> (Float -> String) -> List Bezier.Spline -> String -> String
sectionKeyFrames start now end name toStr splines rendered =
    case splines of
        [] ->
            rendered

        top :: [] ->
            let
                splineStart =
                    Time.millis (Bezier.firstX top)

                -- percentage is calculated from
                -- the later of start time or now
                -- and the end time
                percentage =
                    String.fromFloat (Time.progress start end splineStart * 100) ++ "%"
                

                frame =
                    percentage
                        ++ "{\n    "
                        ++ (name ++ ":" ++ toStr (Bezier.firstY top) ++ ";\n")
                        ++ ("    animation-timing-function:" ++ Bezier.cssTimingString top ++ ";")
                        ++ "\n}\n"

                last =
                    "100% { " ++ (name ++ ":" ++ toStr (Bezier.lastY top) ++ ";}")
                    
            in
            (rendered ++ frame ++ last)

        top :: remaining ->
            if Bezier.afterLastX (Time.inMilliseconds now) top then
                sectionKeyFrames start
                    now
                    end
                    name
                    toStr
                    remaining
                    rendered

            else
                let
                    splineStart =
                        Time.millis (Bezier.firstX top)

                    -- percentage is calculated from
                    -- the later of start time or now
                    -- and the end time
                    percentage =
                        String.fromFloat (Time.progress start end splineStart * 100) ++ "%"
                    
                    frame =
                        percentage
                            ++ "{\n    "
                            ++ (name ++ ":" ++ toStr (Bezier.firstY top) ++ ";\n")
                            ++ ("    animation-timing-function:" ++ Bezier.cssTimingString top ++ ";")
                            ++ "\n}\n"
                in
                sectionKeyFrames start
                    now
                    end
                    name
                    toStr
                    remaining
                    (rendered ++ frame)


infinite : String
infinite =
    "infinite"


splineListHash : List Bezier.Spline -> String -> String
splineListHash splines str =
    case splines of
        [] ->
            str

        top :: remain ->
            splineListHash remain (str ++ Bezier.hash top)


{-| Colors ->
r,g,b,a -> Quad

    We need either rgb, or rgba

Opacity ->
Single

Transform ->
Normally:
x,y,rotation,scale

    Sometimes:
    x,y,z,rotation,scaleX,scaleY,scaleZ,facingX,facingY,facingZ

-}
renderCss : Time.Absolute -> List Renderer -> List RenderedProp -> CssAnim
renderCss now renderFns props =
    renderCssHelper now
        renderFns
        props
        emptyAnim


emptyAnim : CssAnim
emptyAnim =
    { hash = ""
    , animation = ""
    , keyframes = ""
    }


renderCssHelper : Time.Absolute -> List Renderer -> List RenderedProp -> CssAnim -> CssAnim
renderCssHelper now renderer props cssAnim =
    case renderer of
        [] ->
            cssAnim

        render :: remain ->
            case props of
                [] ->
                    cssAnim

                _ ->
                    case render now props of
                        Nothing ->
                            renderCssHelper now
                                remain
                                props
                                cssAnim

                        Just ( newCss, newProps ) ->
                            renderCssHelper now
                                remain
                                newProps
                                (combine newCss cssAnim)


{-|

  - Ids of props to render
  - What props are present at this state

In some cases like `Color`, multiple props need to be rendered as one prop. Same with `transform`.

Deciding how they should combine can be done after this fist pass.

Properties:

  - List RenderedProp is in the same order as List Prop
  - Each `RenderedProp` will have the same number of sections
  - sections are ordered by starting time.

-}
propsToCurves :
    List Prop
    -> (state -> List Prop)
    -> Timeline.Timeline state
    -> List RenderedProp
propsToCurves only lookup timeline =
    Timeline.foldpAll lookup (toPropCurves only) timeline
        |> .rendered


{-| -}
toPropCurves :
    List Prop
    ->
        Timeline.Interp state
            (List Prop)
            { rendered : List RenderedProp
            , previous : Maybe (Timeline.Occurring state)
            }
toPropCurves only =
    { start =
        \props ->
            { rendered =
                List.map
                    (\(Prop onlyId _) ->
                        case matchId onlyId props of
                            Just (Prop id move) ->
                                RenderedProp
                                    { id = id
                                    , default = Internal.Css.Props.default id
                                    , sections = []
                                    , state = Interpolate.moving.start move
                                    }

                            Nothing ->
                                let
                                    default = Internal.Css.Props.default onlyId
                                in
                                RenderedProp
                                    { id = onlyId
                                    , default = default
                                    , sections = []
                                    , state =
                                        Interpolate.moving.start
                                            default
                                    }
                    )
                    only
            , previous = Nothing
            }
    , adjustor =
        \_ ->
            Timeline.linearDefault
    , dwellPeriod =
        \_ ->
            Nothing
    , visit =
        \lookup target targetTime maybeLookAhead data ->
            { rendered =
                List.map
                    (\(RenderedProp rendered) ->
                        RenderedProp
                            { id = rendered.id
                            , default = rendered.default
                            , sections =
                                toCurvesVisit
                                    (\state ->
                                        lookup state
                                            |> stateOrDefault rendered
                                    )
                                    target
                                    targetTime
                                    data.previous
                                    (Maybe.map
                                        (Timeline.mapLookAhead
                                            (stateOrDefault rendered)
                                        )
                                        maybeLookAhead
                                    )
                                    rendered.state
                                    rendered.sections
                            , state =
                                Interpolate.moving.visit
                                    (\state ->
                                        lookup state
                                            |> stateOrDefault rendered
                                    )
                                    target
                                    targetTime
                                    (Maybe.map
                                        (Timeline.mapLookAhead
                                            (stateOrDefault rendered)
                                        )
                                        maybeLookAhead
                                    )
                                    rendered.state
                            }
                    )
                    data.rendered
            , previous = Just target
            }
    , lerp =
        \prevEndTime prev target targetTime interruptedAt maybeLookAhead data ->
            { rendered =
                List.map
                    (\(RenderedProp rendered) ->
                        RenderedProp
                            { id = rendered.id
                            , default = rendered.default
                            , sections =
                                toCurvesLerp
                                    prevEndTime
                                    (stateOrDefault rendered prev)
                                    (stateOrDefault rendered target)
                                    targetTime
                                    interruptedAt
                                    (Maybe.map
                                        (Timeline.mapLookAhead
                                            (stateOrDefault rendered)
                                        )
                                        maybeLookAhead
                                    )
                                    rendered.state
                                    rendered.sections
                            , state =
                                Interpolate.moving.lerp
                                    prevEndTime
                                    (stateOrDefault rendered prev)
                                    (stateOrDefault rendered target)
                                    targetTime
                                    interruptedAt
                                    (Maybe.map
                                        (Timeline.mapLookAhead
                                            (stateOrDefault rendered)
                                        )
                                        maybeLookAhead
                                    )
                                    rendered.state
                            }
                    )
                    data.rendered
            , previous = Nothing
            }
    }


stateOrDefault : RenderedPropDetails -> List Prop -> Interpolate.Movement
stateOrDefault details props =
    case props of
        [] ->
            details.default

        (Prop id move) :: remain ->
            if id == details.id then
                move

            else
                stateOrDefault details remain


matchId : Id -> List Prop -> Maybe Prop
matchId onlyId props =
    case props of
        [] ->
            Nothing

        ((Prop id _) as top) :: remain ->
            if id == onlyId then
                Just top

            else
                matchId onlyId remain


{-| This is a fully composed css proeprty string, such as:

    "background-color: rgb(0,0,0);"

You can include multiple properties if necessary
and we don't have the allocation of an intermediate datastruture like

    [ ( "background-color", "rgb(0,0,0)" ) ]

-}
type alias CssPropString =
    String


{-| Same as above, but we take it all the way to CSS.

So we need:

  - Props to render
  - What props are present at this state
  - How do we render these props as css?

-}
propsToCss :
    List Prop
    -> (state -> List Prop)
    -> (List ( Id, Float ) -> CssPropString)
    -> Timeline.Timeline state
    -> CssAnim
propsToCss only lookup render timeline =
    Debug.todo "Maybe this is the way to go :thinking:"


{-| A group of curves represents the trail of one scalar property

    (Scalar property meaning something like opacity, or just the `R` channel of rgb.)

-}
type RenderedProp
    = RenderedProp RenderedPropDetails


type alias RenderedPropDetails =
    { id : Id
    , default : Interpolate.Movement
    , sections : List Section
    , state : Interpolate.State
    }


{-| A section is one segment of a scalar's journey that can be repeated.

Every state transition will be a separate section.

A dwell will be a section by itself and can possibly repeat.

-}
type Section
    = Section
        { start : Time.Absolute
        , period : Timeline.Period
        , splines : List Bezier.Spline
        }


curves :
    (state -> Interpolate.Movement)
    -> Timeline.Timeline state
    -> List Section
curves lookup timeline =
    Timeline.foldpAll lookup toCurves timeline
        |> .curves



{- TO CURVES -}


{-| -}
toCurves :
    Timeline.Interp state
        Interpolate.Movement
        { curves : List Section
        , previous : Maybe (Timeline.Occurring state)
        , state : Interpolate.State
        }
toCurves =
    { start =
        \motion ->
            { curves = []
            , previous = Nothing
            , state =
                Interpolate.moving.start
                    motion
            }
    , adjustor =
        \_ ->
            Timeline.linearDefault
    , dwellPeriod =
        \_ ->
            Nothing
    , visit =
        \lookup target targetTime maybeLookAhead data ->
            { curves =
                toCurvesVisit
                    lookup
                    target
                    targetTime
                    data.previous
                    maybeLookAhead
                    data.state
                    data.curves
            , previous = Just target
            , state =
                Interpolate.moving.visit
                    lookup
                    target
                    targetTime
                    maybeLookAhead
                    data.state
            }
    , lerp =
        \prevEndTime prev target targetTime interruptedAt maybeLookAhead data ->
            { curves =
                toCurvesLerp
                    prevEndTime
                    prev
                    target
                    targetTime
                    interruptedAt
                    maybeLookAhead
                    data.state
                    data.curves
            , previous = Nothing
            , state =
                Interpolate.moving.lerp
                    prevEndTime
                    prev
                    target
                    targetTime
                    interruptedAt
                    maybeLookAhead
                    data.state
            }
    }


toCurvesVisit :
    (state -> Interpolate.Movement)
    -> Timeline.Occurring state
    -> Time.Absolute
    -> Maybe (Timeline.Occurring state)
    -> Maybe (Timeline.LookAhead Interpolate.Movement)
    -> Interpolate.State
    -> List Section
    -> List Section
toCurvesVisit lookup target targetTime maybePrevious maybeLookAhead state existingSections =
    case maybePrevious of
        Nothing ->
            existingSections

        Just prev ->
            let
                visitSplines =
                    Interpolate.lerpSplines
                        (Timeline.endTime prev)
                        (lookup (Timeline.getEvent prev))
                        (lookup (Timeline.getEvent target))
                        targetTime
                        maybeLookAhead
                        state

                sections =
                    case existingSections of
                        [] ->
                            [(Section
                                { start = Timeline.endTime (Debug.log "STAAART2" prev)
                                , period = once (Time.duration (Timeline.endTime prev) targetTime)
                                , splines = visitSplines
                                }
                            )]
                        (Section top) :: remain ->
                            case top.period of
                                Timeline.Repeat 1 dur ->
                                    let
                                        _ = Debug.log "Combining" (top.start, Timeline.endTime prev)
                                    in
                                    (Section 
                                        { start = top.start
                                        , period = once (Time.expand dur (Time.duration (Timeline.endTime prev) targetTime))
                                        , splines = top.splines ++ visitSplines
                                        }
                                    )
                                     :: remain 
                                        
                                _ ->
                                    existingSections
                                        |> (::)
                                            (Section
                                                { start = Timeline.endTime (Debug.log "STAAART" prev)
                                                , period = once (Time.duration (Timeline.endTime prev) targetTime)
                                                , splines = visitSplines
                                                }
                                            )
            in
            case maybeLookAhead of
                Nothing ->
                    dwellSplines lookup
                        target
                        targetTime
                        sections

                Just _ ->
                    sections


once =
    Timeline.Repeat 1


toCurvesLerp :
    Time.Absolute
    -> Interpolate.Movement
    -> Interpolate.Movement
    -> Time.Absolute
    -> Time.Absolute
    -> Maybe (Timeline.LookAhead Interpolate.Movement)
    -> Interpolate.State
    -> List Section
    -> List Section
toCurvesLerp prevEndTime prev target targetTime interruptedAt maybeLookAhead state existingSections =
    -- finalize current stack
    -- create and finalizeTransition stack for the interruption
    -- (but use the special transition finalizer which embeds timing outside of keyframes)
    let
        -- _ =
            -- Debug.log "LERP DATA"
            --     { data = state
            --     , interruptedAt = interruptedAt
            --     }

        transitionSplines =
            Interpolate.lerpSplines
                prevEndTime
                prev
                target
                targetTime
                maybeLookAhead
                state

        sliced =
            if interruptedAt == targetTime then
                transitionSplines

            else
                Interpolate.takeBefore interruptedAt transitionSplines
    in
    existingSections
        |> (::)
            (Section
                { start = prevEndTime
                , period = once (Time.duration prevEndTime targetTime)
                , splines = sliced
                }
            )


dwellSplines :
    (state -> Interpolate.Movement)
    -> Timeline.Occurring state
    -> Time.Absolute
    -> List Section
    -> List Section
dwellSplines lookup target startTime existing =
    case lookup (Timeline.getEvent target) of
        Interpolate.Osc personality startPos period checkpoints ->
            Section
                { start = startTime
                , period =
                    period
                , splines =
                    List.filterMap
                        (\point ->
                            case point.timing of
                                Interpolate.Linear ->
                                    -- TODO: DO THIS ONE!
                                    Nothing

                                Interpolate.Bezier spline ->
                                    Just (Bezier.addX (Time.inMilliseconds startTime) spline)
                        )
                        checkpoints
                }
                :: existing

        Interpolate.Pos _ _ ->
            existing


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
    case Timeline.foldpAll lookup (toCss now name renderValue lookup) timeline of
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
    case stack.stack of
        [] ->
            { hash = ""
            , animation = ""
            , keyframes = ""
            }

        _ ->
            let
                animationName =
                    name
                        ++ "-"
                        ++ checkpointHash
                            renderValue
                            stack.stack
                            ""

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
                    (durationStr ++ " ")
                        -- we specify an easing function here because it we have to
                        -- , but it is overridden by the one in keyframes
                        ++ "linear "
                        ++ delay
                        ++ " 1 normal forward running "
                        ++ animationName

                keyframes =
                    ("@keyframes " ++ animationName ++ " {\n")
                        ++ checkpointKeyframes name
                            renderValue
                            stack.stack
                            ""
                        ++ "\n}"
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
                            { stackStart = prevEndTime
                            , stackEnd = targetTime
                            , stack =
                                normalizeToCheckpoints
                                    (Time.duration
                                        prevEndTime
                                        targetTime
                                    )
                                    transitionSplines
                            }
                        )
            , stackStart = targetTime
            , stackEnd = targetTime
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
                    (durationStr ++ " ")
                        -- we specify an easing function here because it we have to
                        -- , but it is overridden by the one in keyframes
                        ++ "linear "
                        ++ (delay ++ " ")
                        ++ iterationCount
                        ++ " normal forward running "
                        ++ animationName

                keyframes =
                    "@keyframes "
                        ++ animationName
                        ++ "{"
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


checkpointHash : (Float -> String) -> List Interpolate.Checkpoint -> String -> String
checkpointHash renderValue checkpoints rendered =
    case checkpoints of
        [] ->
            rendered

        top :: remaining ->
            let
                frame =
                    String.fromInt (round top.time)
                        ++ "-"
                        ++ renderValue top.value
                        ++ "-"
                        ++ renderTimingHash top.timing
            in
            checkpointHash renderValue
                remaining
                (rendered ++ frame)


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
                        ++ "{\n    "
                        ++ (name ++ ":" ++ renderValue top.value ++ ";\n")
                        ++ ("    animation-timing-function:" ++ renderTiming top.timing ++ ";")
                        ++ "\n}\n"
            in
            checkpointKeyframes name
                renderValue
                remaining
                (rendered ++ frame)


renderTimingHash : Interpolate.Timing -> String
renderTimingHash timing =
    case timing of
        Interpolate.Linear ->
            "l"

        Interpolate.Bezier (Bezier.Spline c0 c1 c2 c3) ->
            -- the spline passed here needs to be normalized over 0-1
            -- and then we only need to pass the two control points to the css animation
            "bz-"
                ++ (encodeFloat c1.x ++ "-")
                ++ (encodeFloat c1.y ++ "-")
                ++ (encodeFloat c2.x ++ "-")
                ++ encodeFloat c2.y


encodeFloat fl =
    String.fromInt (round fl)


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



-- compoundSequence :
--     (state -> List ( Key, Interpolate.Movement ))
--     ->
--         Timeline.Interp state
--             Interpolate.Movement
--             { sequence : Sequence (Dict Key Interpolate.Checkpoint)
--             , transformConflict : Bool
--             , keys : Set Key
--             }
-- compoundSequence toMotion =
--     Debug.todo ""
-- {-| render keyframes to string
--     p {
--         animation-duration: 3s;
--         animation-name: one;
--         animation-delay: 300ms;
--         animation-iteration-count: infinite | 5;
--     }
--     @keyframes transitionOne {
--         0% {
--             -- we can define the timing function which says how to interpolate from this keyframe to the next
--             animation-timing-function: cubic-bezier(0.19, 1, 0.22, 1);
--             transform: translate(0px, 0px)
--         }
--         100% {
--             transform: translate(1000px, 1000px)
--         }
--     }
-- Transitions:
-- When doing transitions, we can't add a `timing-fn` to a keyframe with no values, which would have been perfect.
-- Instead, for each transition, we need to compose a full animation
-- Example: <https://codepen.io/mechanical-elephant/pen/MWjEXzq>
--     @keyframes normal {
--         from {
--             transform: translateX(0px);
--         }
--         to {
--             transform: translateX(200px);
--         }
--     }
--     @keyframes transition {
--         to {
--             transform: translateX(1200px);
--         }
--     }
--     /* The element to apply the animation to */
--     .item {
--         width: 100px;
--         height: 100px;
--         background-color:red;
--         transform: translateX(0px);
--         animation-timing-function: linear, cubic-bezier(0.1, -0.6, 0.2, 0);
--         animation-name: normal, transition;
--         animation-delay: 0ms, 4s;
--         animation-duration: 8s, 4s;
--     }
-- Each property, and each "set" will render as a separate keyframe statement
-- -}
-- encode :
--     (frame
--      ->
--         -- this is timing to get to this state
--         -- for transitions, this turns out to be what we want
--         -- for normal keyframes, we have to shift it back one step.
--         { timing : Interpolate.Timing
--         , name : String
--         , value : String
--         }
--     )
--     -> Sequence frame
--     -> CssAnim
-- encode fn (Sequence seq) =
--     { hash = ""
--     , animation = ""
--     , keyframes = ""
--     }
-- {-| A less efficient version of the above that encodes every frame as a keyframe.
-- returned frames will be distributed evenly.
-- -}
-- encodeEveryFrame :
--     (frame
--      ->
--         -- this is timing to get to this state
--         -- for transitions, this turns out to be what we want
--         -- for normal keyframes, we have to shift it back one step.
--         List
--             { name : String
--             , value : String
--             }
--     )
--     -> Sequence frame
--     -> CssAnim
-- encodeEveryFrame fn (Sequence seq) =
--     { hash = ""
--     , animation = ""
--     , keyframes = ""
--     }


combine : CssAnim -> CssAnim -> CssAnim
combine one two =
    if String.isEmpty one.hash then
        two

    else if String.isEmpty two.hash then
        one

    else
        { hash = one.hash ++ two.hash
        , animation = one.animation ++ ", " ++ two.animation
        , keyframes = one.keyframes ++ "\n" ++ two.keyframes
        }
