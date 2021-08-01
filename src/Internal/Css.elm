module Internal.Css exposing (..)

{-| -}

import Color
import Duration
import Html.Attributes exposing (id)
import Internal.Bezier as Bezier
import Internal.Css.Props
import Internal.Interpolate as Interpolate
import Internal.Move as Move
import Internal.Time as Time
import Internal.Timeline as Timeline
import Internal.Transition as Transition
import Internal.Units as Units
import Pixels
import Quantity
import Set exposing (Set)


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
      -- they are only really necessary for `transforms`
      -- props defined by the user use the prop name for identity
      Prop Id String (Move.Move Float) Internal.Css.Props.Format
    | ColorProp String (Move.Move Float) Color.Color


applyToMovement : (Move.Move Float -> Move.Move Float) -> Prop -> Prop
applyToMovement fn prop =
    case prop of
        Prop id name m format ->
            Prop id name (fn m) format

        ColorProp name movement clr ->
            ColorProp name (fn movement) clr


type alias ColorPropDetails =
    { name : String
    , color : Color.Color
    }


add :
    List Prop
    -> List Prop
    -> Set String
    -> Set Id
    ->
        { props : List Prop
        , cache :
            { name : Set String
            , id : Set Id
            }
        }
add new existingProps names ids =
    case new of
        [] ->
            { props = existingProps
            , cache =
                { name = names
                , id = ids
                }
            }

        ((Prop id name _ _) as prop) :: remain ->
            if (id - Internal.Css.Props.noId) == 0 then
                if Set.member name names then
                    add remain existingProps names ids

                else
                    add remain
                        (prop :: existingProps)
                        (Set.insert name names)
                        ids

            else if Set.member id ids then
                add remain existingProps names ids

            else
                add remain
                    (prop :: existingProps)
                    names
                    (Set.insert id ids)

        ((ColorProp name movement clr) as prop) :: remain ->
            if Set.member name names then
                add remain existingProps names ids

            else
                add remain
                    (prop :: existingProps)
                    (Set.insert name names)
                    ids


propOrder : Prop -> Int
propOrder prop =
    case prop of
        Prop id _ _ _ ->
            id

        ColorProp _ _ _ ->
            Internal.Css.Props.noId


cssFromProps : Timeline.Timeline state -> (state -> List Prop) -> CssAnim
cssFromProps timeline lookup =
    let
        present =
            getInitial2 timeline lookup

        renderedProps =
            Timeline.foldpAll2 lookup
                (\_ -> present)
                toPropCurves2
                timeline
    in
    renderCss (Timeline.getCurrentTime timeline) renderers renderedProps


getInitial2 : Timeline.Timeline event -> (event -> List Prop) -> List RenderedProp
getInitial2 timeline lookup =
    let
        ( maybeTransform, renderedProps ) =
            Timeline.foldpAll2 lookup
                (\props ->
                    toInitialProps props ( Nothing, [] )
                )
                (\get prev target now startTime endTime future cursor ->
                    addInitialProps (get (Timeline.getEvent target)) cursor
                )
                timeline
    in
    case maybeTransform of
        Nothing ->
            renderedProps

        Just trans ->
            TransformProp trans :: renderedProps


initState x =
    { position =
        Pixels.pixels x
    , velocity = Pixels.pixelsPerSecond 0
    }


toInitialProps : List Prop -> ( Maybe TransformPropDetails, List RenderedProp ) -> ( Maybe TransformPropDetails, List RenderedProp )
toInitialProps props (( maybeTransform, rendered ) as untouched) =
    case props of
        [] ->
            untouched

        (Prop id name movement format) :: remaining ->
            let
                state =
                    Interpolate.moving.start
                        (Internal.Css.Props.default id)
            in
            toInitialProps remaining
                (if Internal.Css.Props.isTransformId id then
                    case maybeTransform of
                        Nothing ->
                            ( Just
                                { sections = []
                                , state =
                                    { x = initState 0
                                    , y = initState 0
                                    , scale = initState 1
                                    , rotation = initState 0
                                    }
                                }
                            , rendered
                            )

                        Just trans ->
                            -- we've already initialized the transform
                            untouched

                 else
                    ( maybeTransform
                    , RenderedProp
                        { id = id
                        , name = name
                        , format = format
                        , sections = []
                        , state = state
                        }
                        :: rendered
                    )
                )

        (ColorProp name movement color) :: remaining ->
            toInitialProps remaining
                ( maybeTransform
                , RenderedColorProp
                    { name = name
                    , color = color
                    , sections = []
                    }
                    :: rendered
                )


matchProp id renderedProp =
    case renderedProp of
        RenderedProp details ->
            details.id - id == 0

        CompoundProp details ->
            False

        RenderedColorProp details ->
            False

        TransformProp details ->
            False


matchColor name renderedProp =
    case renderedProp of
        RenderedProp details ->
            False

        CompoundProp details ->
            False

        RenderedColorProp details ->
            details.name == name

        TransformProp details ->
            False


{-| If a props isn't defined in the first state, but is defined in the future, we want to add it.
-}
addInitialProps : List Prop -> ( Maybe TransformPropDetails, List RenderedProp ) -> ( Maybe TransformPropDetails, List RenderedProp )
addInitialProps props (( maybeTransform, rendered ) as untouched) =
    case props of
        [] ->
            untouched

        (Prop id name movement format) :: remaining ->
            let
                new =
                    if Internal.Css.Props.isTransformId id then
                        case maybeTransform of
                            Nothing ->
                                ( Just
                                    { sections = []
                                    , state =
                                        { x = initState 0
                                        , y = initState 0
                                        , scale = initState 1
                                        , rotation = initState 0
                                        }
                                    }
                                , rendered
                                )

                            Just trans ->
                                -- we've already initialized the transform
                                untouched

                    else if List.any (\renderedProp -> matchProp id renderedProp) rendered then
                        untouched

                    else
                        let
                            state =
                                Interpolate.moving.start
                                    (Internal.Css.Props.default id)
                        in
                        ( maybeTransform
                        , RenderedProp
                            { id = id
                            , name = name
                            , format = format
                            , sections = []
                            , state = state
                            }
                            :: rendered
                        )
            in
            toInitialProps remaining
                new

        (ColorProp name movement color) :: remaining ->
            let
                new =
                    if List.any (\renderedProp -> matchColor name renderedProp) rendered then
                        untouched

                    else
                        ( maybeTransform
                        , RenderedColorProp
                            { name = name
                            , color = color
                            , sections = []
                            }
                            :: rendered
                        )
            in
            toInitialProps remaining
                new


getInitial : Timeline.Timeline event -> (event -> List Prop) -> List Prop -> List RenderedProp
getInitial timeline lookup =
    Timeline.foldpAll2 lookup
        (\props ->
            add props [] Set.empty Set.empty
        )
        (\get prev target now startTime endTime future cursor ->
            let
                new =
                    case cursor.props of
                        [] ->
                            get (Timeline.getEvent target) ++ get (Timeline.getEvent prev)

                        _ ->
                            get (Timeline.getEvent target)
            in
            add new
                cursor.props
                cursor.cache.name
                cursor.cache.id
        )
        timeline
        |> .props
        |> List.sortBy propOrder
        |> startProps


{-| RenderdProp's are required to be ordered!
-}
type alias Renderer =
    Time.Absolute -> List RenderedProp -> Maybe ( CssAnim, List RenderedProp )


renderers : List Renderer
renderers =
    [ transform
    , scalars
    ]


multiColorHash :
    List
        { percent : Int
        , color : Color.Color
        }
    -> String
    -> String
multiColorHash colorList str =
    case colorList of
        [] ->
            str

        step :: remain ->
            multiColorHash remain
                (Internal.Css.Props.colorHash step.color
                    ++ str
                )


colorFrames :
    String
    ->
        List
            { percent : Int
            , color : Color.Color
            }
    -> String
    -> String
colorFrames name colorList str =
    case colorList of
        [] ->
            str

        step :: remain ->
            colorFrames name
                remain
                (str
                    ++ (String.fromInt step.percent ++ "% {")
                    ++ (name ++ ": " ++ Color.toCssString step.color)
                    ++ "}"
                )


transform : Renderer
transform now renderedProps =
    case renderedProps of
        [] ->
            Nothing

        (CompoundProp comp) :: remain ->
            let
                rendered =
                    renderCompoundSections now comp.slices emptyAnim
            in
            if isEmptyAnim rendered then
                Just
                    ( { props = [ ( "transform", renderTransformProp comp.states "" ) ]
                      , keyframes = ""
                      , hash = ""
                      , animation = ""
                      }
                    , remain
                    )

            else
                Just
                    ( rendered
                    , remain
                    )

        _ ->
            Nothing


renderCompoundSections : Time.Absolute -> List CompoundSection -> CssAnim -> CssAnim
renderCompoundSections now sections anim =
    case sections of
        [] ->
            anim

        targetSection :: tail ->
            let
                end =
                    targetSection.start
                        |> Time.advanceBy (Timeline.periodDuration targetSection.period)

                onlyOnce =
                    Timeline.isOnce targetSection.period
            in
            if onlyOnce && Time.thisBeforeThat end now then
                renderCompoundSections now
                    tail
                    anim

            else
                let
                    ( section, remain ) =
                        if not (Time.equal now targetSection.start) && Timeline.isDuring now targetSection.start targetSection.period then
                            let
                                ( old_, active, maybeNext ) =
                                    splitCompoundSection now targetSection
                            in
                            ( active
                            , case maybeNext of
                                Nothing ->
                                    tail

                                Just next ->
                                    next :: tail
                            )

                        else
                            ( targetSection, tail )

                    new =
                        if section.conflicting then
                            renderCompoundKeyframesExact 12
                                section.start
                                section.period
                                section.frames
                                ""
                                ""

                        else
                            renderCompoundKeyframes
                                section.start
                                section.period
                                section.frames
                                ""
                                ""

                    duration =
                        case section.period of
                            Timeline.Loop dur ->
                                dur

                            Timeline.Repeat _ dur ->
                                dur

                    durationStr =
                        (Duration.inMilliseconds duration
                            |> round
                            |> String.fromInt
                        )
                            ++ "ms"

                    n =
                        case section.period of
                            Timeline.Loop _ ->
                                infinite

                            Timeline.Repeat count _ ->
                                String.fromInt count

                    delay =
                        Time.duration now section.start
                            |> Duration.inMilliseconds
                            |> round
                            |> String.fromInt
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
                            ++ " "
                            ++ n
                            ++ " normal forwards running "
                            ++ new.hash
                in
                renderCompoundSections now
                    remain
                    (combine
                        { hash = new.hash
                        , animation = animation
                        , keyframes =
                            "@keyframes "
                                ++ new.hash
                                ++ " {\n"
                                ++ new.keyframes
                                ++ "\n}"
                        , props = []
                        }
                        anim
                    )


keyframeHash :
    List
        { id : Id
        , spline : Bezier.Spline
        }
    -> String
    -> String
keyframeHash keyframes rendered =
    case keyframes of
        [] ->
            rendered

        top :: remain ->
            keyframeHash remain
                (Internal.Css.Props.hash top.id ++ Bezier.hash top.spline ++ rendered)


{-|

    keyframe is a snapshot between start and end.

    render perecentages using Keyframe accordingly.

-}
renderAllExactFrames :
    Time.Duration
    -> Time.Absolute
    -> Time.Absolute
    -> Time.Absolute
    -> Keyframe
    -> String
    -> String
renderAllExactFrames duration start end now keyframe rendered =
    if Time.thisAfterOrEqualThat now keyframe.end then
        rendered

    else
        let
            next =
                now |> Time.advanceBy duration

            percentage =
                round (Time.progress start end now * 100)

            newFrame =
                (String.fromInt percentage ++ "% {\n")
                    ++ "        transform: "
                    ++ renderPointAt (Time.inMilliseconds now)
                        keyframe.props
                        ""
                    ++ ";}"
        in
        if Time.thisAfterOrEqualThat next keyframe.end then
            let
                lastPercentage =
                    round (Time.progress start end next * 100)

                lastFrame =
                    (String.fromInt percentage ++ "% {\n")
                        ++ "        transform: "
                        ++ renderPointAt (Time.inMilliseconds next)
                            keyframe.props
                            ""
                        ++ ";}"
            in
            rendered ++ newFrame ++ lastFrame

        else
            renderAllExactFrames duration
                start
                end
                next
                keyframe
                (rendered ++ "    " ++ newFrame)


{-| -}
renderCompoundKeyframesExact :
    Int
    -> Time.Absolute
    -> Timeline.Period
    -> List Keyframe
    -> String
    -> String
    ->
        { hash : String
        , keyframes : String
        }
renderCompoundKeyframesExact fps start period keyframeList hash rendered =
    case keyframeList of
        [] ->
            { hash = hash
            , keyframes = rendered
            }

        keyframe :: remain ->
            let
                end =
                    start |> Time.advanceBy (Timeline.periodDuration period)

                msPerFrame =
                    Duration.milliseconds (1 / (toFloat fps / 1000))

                frames =
                    renderAllExactFrames
                        msPerFrame
                        start
                        end
                        start
                        keyframe
                        ""
            in
            renderCompoundKeyframesExact fps
                start
                period
                remain
                (keyframeHash keyframe.props "" ++ hash)
                (rendered ++ frames)


renderCompoundKeyframes :
    Time.Absolute
    -> Timeline.Period
    -> List Keyframe
    -> String
    -> String
    ->
        { hash : String
        , keyframes : String
        }
renderCompoundKeyframes start period frames hash rendered =
    case frames of
        [] ->
            { hash = hash
            , keyframes = rendered
            }

        keyframe :: remain ->
            let
                percentage =
                    round
                        (100
                            * Time.progress start
                                (start |> Time.advanceBy (Timeline.periodDuration period))
                                keyframe.start
                        )

                frame =
                    "    "
                        -- This is for debugging, to see the literal time generated
                        -- ++ String.fromFloat (Time.inMilliseconds keyframe.start)
                        -- ++ " -- "
                        ++ (String.fromInt percentage ++ "% {\n")
                        ++ "        transform: "
                        ++ renderFirstPoint
                            keyframe.props
                            ""
                        ++ ";\n"
                        ++ "        animation-timing-function:"
                        ++ Bezier.cssTimingString keyframe.timing
                        ++ ";\n    }"

                last =
                    case remain of
                        [] ->
                            "    "
                                -- This is for debugging, to see the literal time generated
                                -- ++ String.fromFloat (Time.inMilliseconds keyframe.end)
                                -- ++ " -- "
                                ++ "100% { transform: "
                                ++ renderLastPoint
                                    keyframe.props
                                    ""
                                ++ ";\n    }"

                        _ ->
                            ""
            in
            renderCompoundKeyframes start
                period
                remain
                (keyframeHash keyframe.props "" ++ hash)
                (rendered ++ frame ++ "\n" ++ last)


renderPointAt :
    Float
    ->
        List
            { id : Internal.Css.Props.Id
            , spline : Bezier.Spline
            }
    -> String
    -> String
renderPointAt now props rendered =
    case props of
        [] ->
            rendered

        prop :: remain ->
            let
                renderValue =
                    Internal.Css.Props.toStr prop.id

                val =
                    renderValue
                        (Bezier.atX now prop.spline
                            |> .point
                            |> .y
                        )

                new =
                    case rendered of
                        "" ->
                            val

                        _ ->
                            rendered ++ " " ++ val
            in
            renderPointAt now remain new


renderTransformProp : List ( Id, Interpolate.State ) -> String -> String
renderTransformProp states rendered =
    case states of
        [] ->
            rendered

        ( id, state ) :: remain ->
            renderTransformProp remain
                (Internal.Css.Props.toStr id
                    (Pixels.inPixels state.position)
                    ++ " "
                    ++ rendered
                )


renderFirstPoint :
    List
        { id : Internal.Css.Props.Id
        , spline : Bezier.Spline
        }
    -> String
    -> String
renderFirstPoint props rendered =
    case props of
        [] ->
            rendered

        prop :: remain ->
            let
                renderValue =
                    Internal.Css.Props.toStr prop.id

                val =
                    renderValue (Bezier.firstY prop.spline)

                new =
                    case rendered of
                        "" ->
                            val

                        _ ->
                            rendered ++ " " ++ val
            in
            renderFirstPoint remain new


renderLastPoint :
    List
        { id : Internal.Css.Props.Id
        , spline : Bezier.Spline
        }
    -> String
    -> String
renderLastPoint props rendered =
    case props of
        [] ->
            rendered

        prop :: remain ->
            let
                renderValue =
                    Internal.Css.Props.toStr prop.id

                val =
                    renderValue (Bezier.lastY prop.spline)

                new =
                    case rendered of
                        "" ->
                            val

                        _ ->
                            rendered ++ " " ++ val
            in
            renderLastPoint remain new


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

        (RenderedProp details) :: remain ->
            scalarHelper now
                remain
                (propToCssHelper now
                    (Pixels.inPixels details.state.position)
                    details
                    (List.reverse details.sections)
                    emptyAnim
                    |> combine anim
                )

        (CompoundProp _) :: remain ->
            -- handled by transform renderer
            scalarHelper now remain anim

        (RenderedColorProp details) :: remain ->
            scalarHelper now
                remain
                (colorToCssHelper now
                    details.color
                    details
                    (List.reverse details.sections)
                    emptyAnim
                    |> combine anim
                )

        (TransformProp details) :: remain ->
            scalarHelper now
                remain
                (transformToCssHelper now
                    (stateToTransform details.state)
                    details
                    (List.reverse details.sections)
                    emptyAnim
                    |> combine anim
                )


transformToCssHelper :
    Time.Absolute
    -> Transform
    -> TransformPropDetails
    ->
        List
            { delay : Duration.Duration
            , sequence : Move.Sequence Transform
            }
    -> CssAnim
    -> CssAnim
transformToCssHelper now startPos details sections anim =
    case sections of
        [] ->
            if isEmptyAnim anim then
                { anim
                    | props =
                        ( "transform"
                        , renderTransformState details.state
                        )
                            :: anim.props
                }

            else
                anim

        sequence :: remain ->
            transformToCssHelper now
                (Move.lastPosOr startPos sequence.sequence)
                details
                remain
                (combine
                    -- NOTE, this order is important!
                    -- it affects the order of the animation statements in CSS
                    -- If they are out of order they can cancel each other out in weird ways.
                    (Move.css now
                        sequence.delay
                        startPos
                        "transform"
                        transformToString
                        transformToHash
                        sequence.sequence
                    )
                    anim
                )


transformToHash trans =
    "t-"
        ++ String.fromInt (round trans.x)
        ++ "-"
        ++ String.fromInt (round trans.y)
        ++ "-"
        ++ Move.floatToString trans.rotation
        ++ "-"
        ++ Move.floatToString trans.scale


stateToTransform state =
    { x =
        Pixels.inPixels state.x.position
    , y =
        Pixels.inPixels state.y.position
    , scale =
        Pixels.inPixels state.scale.position
    , rotation =
        Pixels.inPixels state.rotation.position
    }



-- scalarHelper now remain anim


renderTransformState state =
    "translate("
        ++ String.fromFloat (Pixels.inPixels state.x.position)
        ++ "px, "
        ++ String.fromFloat (Pixels.inPixels state.y.position)
        ++ "px) rotate("
        ++ String.fromFloat (Pixels.inPixels state.rotation.position)
        ++ "turn)"
        ++ " scale("
        ++ String.fromFloat (Pixels.inPixels state.scale.position)
        ++ ")"


transformToString trans =
    "translate("
        ++ String.fromFloat trans.x
        ++ "px, "
        ++ String.fromFloat trans.y
        ++ "px) rotate("
        ++ String.fromFloat trans.rotation
        ++ "turn)"
        ++ " scale("
        ++ String.fromFloat trans.scale
        ++ ")"


colorToCssHelper :
    Time.Absolute
    -> Color.Color
    -> RenderedColorPropDetails
    ->
        List
            { delay : Duration.Duration
            , sequence : Move.Sequence Color.Color
            }
    -> CssAnim
    -> CssAnim
colorToCssHelper now startPos details sections anim =
    case sections of
        [] ->
            if isEmptyAnim anim then
                { anim
                    | props =
                        ( details.name
                        , Color.toCssString details.color
                        )
                            :: anim.props
                }

            else
                anim

        sequence :: remain ->
            colorToCssHelper now
                (Move.lastPosOr startPos sequence.sequence)
                details
                remain
                (combine
                    -- NOTE, this order is important!
                    -- it affects the order of the animation statements in CSS
                    -- If they are out of order they can cancel each other out in weird ways.
                    (Move.css now
                        sequence.delay
                        startPos
                        details.name
                        Color.toCssString
                        Internal.Css.Props.colorHash
                        sequence.sequence
                    )
                    anim
                )


propToCssHelper :
    Time.Absolute
    -> Float
    -> RenderedPropDetails
    ->
        List
            { delay : Duration.Duration
            , sequence : Move.Sequence Float
            }
    -> CssAnim
    -> CssAnim
propToCssHelper now startPos details sections anim =
    case sections of
        [] ->
            if isEmptyAnim anim then
                let
                    value =
                        Pixels.inPixels details.state.position
                            |> Internal.Css.Props.format details.format
                in
                { anim
                    | props =
                        ( details.name
                        , value
                        )
                            :: anim.props
                }

            else
                anim

        sequence :: remain ->
            propToCssHelper now
                (Move.lastPosOr startPos sequence.sequence)
                details
                remain
                (combine
                    -- NOTE, this order is important!
                    -- it affects the order of the animation statements in CSS
                    -- If they are out of order they can cancel each other out in weird ways.
                    (Move.css now
                        sequence.delay
                        startPos
                        details.name
                        (Internal.Css.Props.format details.format)
                        Move.floatToString
                        sequence.sequence
                    )
                    anim
                )


sectionCss : Time.Absolute -> Id -> String -> Internal.Css.Props.Format -> Section -> CssAnim
sectionCss now id name format ((Section details) as section) =
    case details.splines of
        [] ->
            emptyAnim

        _ ->
            let
                end =
                    details.start
                        |> Time.advanceBy (Timeline.periodDuration details.period)

                onlyOnce =
                    Timeline.isOnce details.period
            in
            if onlyOnce && Time.thisBeforeThat end now then
                emptyAnim

            else if Timeline.isDuring now details.start details.period then
                case splitSection now section of
                    ( old_, active, Nothing ) ->
                        sectionToCss now id name format active

                    ( old_, active, Just next ) ->
                        sectionToCss now id name format active
                            |> combine (sectionToCss now id name format next)

            else
                sectionToCss now id name format section


sectionToCss :
    Time.Absolute
    -> Internal.Css.Props.Id
    -> String
    -> Internal.Css.Props.Format
    -> Section
    -> CssAnim
sectionToCss now id name format (Section section) =
    let
        splines =
            section.splines

        toStr f =
            Internal.Css.Props.format format f

        animationName =
            name
                ++ "-"
                ++ splineListHash splines ""

        duration =
            case section.period of
                Timeline.Loop dur ->
                    dur

                Timeline.Repeat _ dur ->
                    dur

        durationStr =
            (Duration.inMilliseconds duration
                |> round
                |> String.fromInt
            )
                ++ "ms"

        delay =
            Time.duration now section.start
                |> Duration.inMilliseconds
                |> round
                |> String.fromInt
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
                ++ " normal forwards running "
                ++ animationName

        keyframes =
            ("@keyframes " ++ animationName ++ " {\n")
                ++ sectionKeyFrames
                    section.start
                    now
                    (Time.advanceBy duration section.start)
                    name
                    toStr
                    splines
                    ""
                ++ "\n}"
    in
    { hash = animationName
    , animation = animation
    , keyframes = keyframes
    , props = []
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
                    String.fromInt (round (Time.progress start end splineStart * 100))
                        ++ "%"

                frame =
                    percentage
                        ++ "{\n    "
                        ++ (name ++ ":" ++ toStr (Bezier.firstY top) ++ ";\n")
                        ++ ("    animation-timing-function:" ++ Bezier.cssTimingString top ++ ";")
                        ++ "\n}\n"

                last =
                    "100% { " ++ (name ++ ":" ++ toStr (Bezier.lastY top) ++ ";}")
            in
            rendered ++ frame ++ last

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
                        String.fromInt (round (Time.progress start end splineStart * 100)) ++ "%"

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


isEmptyAnim : { css | keyframes : String } -> Bool
isEmptyAnim anim =
    case anim.keyframes of
        "" ->
            True

        _ ->
            False


emptyAnim : CssAnim
emptyAnim =
    { hash = ""
    , animation = ""
    , keyframes = ""
    , props = []
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


startProps :
    List Prop
    -> List Prop
    -> List RenderedProp
startProps only props =
    startPropsHelper only props Nothing []


startPropsHelper :
    List Prop
    -> List Prop
    -> Maybe Compound
    -> List RenderedProp
    -> List RenderedProp
startPropsHelper only props maybeTransform rendered =
    case only of
        [] ->
            case maybeTransform of
                Nothing ->
                    List.reverse rendered

                Just cmpd ->
                    CompoundProp cmpd
                        :: List.reverse rendered

        (ColorProp name movement clr) :: remain ->
            let
                new =
                    RenderedColorProp
                        { name = name
                        , color = clr
                        , sections = []
                        }
            in
            startPropsHelper remain props maybeTransform (new :: rendered)

        (Prop onlyId onlyName onlyMove onlyFormat) :: remain ->
            let
                state =
                    case matchForMovement onlyId onlyName props of
                        Nothing ->
                            Interpolate.moving.start
                                (Internal.Css.Props.default onlyId)

                        Just move ->
                            Interpolate.moving.start move
            in
            if Internal.Css.Props.isTransformId onlyId then
                case maybeTransform of
                    Nothing ->
                        let
                            new =
                                { slices = []
                                , states = [ ( onlyId, state ) ]
                                }
                        in
                        startPropsHelper remain props (Just new) rendered

                    Just cmpd ->
                        startPropsHelper remain
                            props
                            (Just { cmpd | states = ( onlyId, state ) :: cmpd.states })
                            rendered

            else
                let
                    new =
                        RenderedProp
                            { id = onlyId
                            , name = onlyName
                            , format = onlyFormat
                            , sections = []
                            , state = state
                            }
                in
                startPropsHelper remain props maybeTransform (new :: rendered)


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


denormalize :
    Time.Absolute
    -> Time.Absolute
    -> Float
    -> Float
    ->
        { position : Bezier.Point
        , velocity : Bezier.Point
        }
    -> Interpolate.State
denormalize startTime targetTime startPosition targetPosition state =
    { position =
        Pixels.pixels
            (Move.toReal
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
        Pixels.pixelsPerSecond (scaled.y / scaled.x)
    }


{-| -}
toPropCurves2 : Timeline.Transition state (List Prop) (List RenderedProp)
toPropCurves2 lookup prev target now startTime endTime future cursor =
    {-
       1. always track `state`
       2. only start to record sections if they're not done
       3. If there are


    -}
    let
        -- _ =
        --     Debug.log "   TO PROPS"
        --         { prev = prev
        --         , now = now
        --         , target = target
        --         }
        targetTime =
            Timeline.startTime target

        progress =
            Time.progress startTime targetTime now

        --startTime =
        --    Timeline.endTime prev
        finished =
            -- we only want to ignore this event if it's both finished
            -- and not immediately preceding an event that is still upcoming
            --case future of
            --    [] ->
            --        False
            --
            --    next :: _ ->
            Time.thisAfterOrEqualThat now (Timeline.endTime target)

        --&& not (Time.thisBeforeThat now (Timeline.startTime next))
    in
    List.map
        (\prop ->
            case prop of
                RenderedColorProp details ->
                    let
                        previousColor =
                            colorOrDefault details.name
                                Internal.Css.Props.transparent
                                (lookup (Timeline.getEvent prev))

                        targetColor =
                            colorOrDefault details.name
                                Internal.Css.Props.transparent
                                (lookup (Timeline.getEvent target))

                        halfTime =
                            startTime
                                |> Time.advanceBy
                                    (Time.duration startTime endTime
                                        |> Quantity.divideBy 2
                                    )
                    in
                    RenderedColorProp
                        { name = details.name
                        , sections =
                            if finished then
                                details.sections

                            else
                                details.sections
                                    |> Move.sequences
                                        startTime
                                        halfTime
                                        now
                                        endTime
                                        (Move.toWith Transition.linear
                                            (Interpolate.color 0.5
                                                previousColor
                                                targetColor
                                            )
                                        )
                                    |> Move.sequences
                                        halfTime
                                        targetTime
                                        now
                                        endTime
                                        (Move.toWith Transition.linear targetColor)
                        , color =
                            -- This should likely be the `current` color instead
                            targetColor
                        }

                RenderedProp rendered ->
                    let
                        targetProp : Move.Move Float
                        targetProp =
                            Timeline.getEvent target
                                |> lookup
                                |> stateOrDefault rendered.id
                                    rendered.name

                        -- adjust the transition if necessary by taking into account
                        -- the intro and exit velocity
                        finalProp =
                            targetProp
                                |> Move.withVelocities
                                    (normalizeVelocity
                                        startTime
                                        targetTime
                                        startPosition
                                        targetPosition
                                        rendered.state.velocity
                                    )
                                    -- If we do any transition smoothing
                                    -- we'll need to normalize this velocity too
                                    --Interpolate.velocityAtTarget lookupState target future
                                    0

                        startPosition =
                            Pixels.inPixels rendered.state.position

                        targetPosition =
                            case targetProp of
                                Move.Pos _ x _ ->
                                    x
                    in
                    RenderedProp
                        { id = rendered.id
                        , name = rendered.name
                        , format = rendered.format
                        , sections =
                            if finished then
                                rendered.sections

                            else
                                Move.sequences
                                    startTime
                                    targetTime
                                    now
                                    endTime
                                    finalProp
                                    rendered.sections
                        , state =
                            Move.atX progress finalProp
                                |> denormalize startTime
                                    targetTime
                                    startPosition
                                    targetPosition
                        }

                TransformProp details ->
                    -- for each prop
                    --  calculate a new state
                    --  calculate a new transition
                    --     (for now), take the most "different" curve
                    --  Compose a new `Move Transform` with the transition
                    --
                    let
                        targetProps : List Prop
                        targetProps =
                            Timeline.getEvent target
                                |> lookup

                        commonTransition : Transition.Transition
                        commonTransition =
                            getCommonTransformTransition targetProps

                        targets =
                            { x =
                                transformOrDefault Internal.Css.Props.ids.x
                                    targetProps
                            , y =
                                transformOrDefault Internal.Css.Props.ids.y
                                    targetProps
                            , scale =
                                transformOrDefault Internal.Css.Props.ids.scale
                                    targetProps
                            , rotation =
                                transformOrDefault Internal.Css.Props.ids.rotation
                                    targetProps
                            }
                    in
                    TransformProp
                        { sections =
                            if finished then
                                details.sections

                            else
                                Move.sequences
                                    startTime
                                    targetTime
                                    now
                                    endTime
                                    (Move.toWith commonTransition
                                        targets
                                    )
                                    details.sections
                        , state =
                            { x =
                                Move.toWith commonTransition
                                    targets.x
                                    |> Move.atX progress
                                    |> denormalize startTime
                                        targetTime
                                        (Pixels.inPixels details.state.x.position)
                                        targets.x
                            , y =
                                Move.toWith commonTransition
                                    targets.y
                                    |> Move.atX progress
                                    |> denormalize startTime
                                        targetTime
                                        (Pixels.inPixels details.state.y.position)
                                        targets.y
                            , scale =
                                Move.toWith commonTransition
                                    targets.scale
                                    |> Move.atX progress
                                    |> denormalize startTime
                                        targetTime
                                        (Pixels.inPixels details.state.scale.position)
                                        targets.scale
                            , rotation =
                                Move.toWith commonTransition
                                    targets.rotation
                                    |> Move.atX progress
                                    |> denormalize startTime
                                        targetTime
                                        (Pixels.inPixels details.state.rotation.position)
                                        targets.rotation
                            }
                        }

                CompoundProp details ->
                    let
                        new =
                            lerpCurvesCompoundHelper2
                                details.states
                                lookup
                                prev
                                target
                                now
                                future
                                False
                                []
                                []
                    in
                    CompoundProp
                        { slices =
                            if finished then
                                details.slices

                            else
                                let
                                    newCompound =
                                        { start = startTime
                                        , period = once (Time.duration startTime targetTime)
                                        , conflicting = new.conflicting
                                        , frames = new.keyframes
                                        }
                                in
                                case details.slices of
                                    [] ->
                                        [ newCompound ]

                                    last :: remain ->
                                        if isCombineableCompoundSections newCompound last then
                                            combineCompound newCompound last :: remain

                                        else
                                            newCompound :: details.slices
                        , states = new.states
                        }
        )
        cursor


{-| -}
stateOrDefaultByName : String -> List Prop -> Move.Move Float
stateOrDefaultByName targetName props =
    case props of
        [] ->
            Internal.Css.Props.zero

        (Prop _ propName move _) :: remain ->
            if propName == targetName then
                move

            else
                stateOrDefaultByName targetName remain

        (ColorProp propName movement clr) :: remain ->
            if propName == targetName then
                movement

            else
                stateOrDefaultByName targetName remain


getCommonTransformTransition : List Prop -> Transition.Transition
getCommonTransformTransition props =
    Transition.standard


{-| -}
transformOrDefault : Id -> List Prop -> Float
transformOrDefault targetId props =
    case props of
        [] ->
            Internal.Css.Props.defaultPosition targetId

        (Prop id name move _) :: remain ->
            if id - targetId == 0 then
                case move of
                    Move.Pos _ v _ ->
                        v

            else
                transformOrDefault targetId remain

        (ColorProp name movement clr) :: remain ->
            transformOrDefault targetId remain


{-| -}
stateOrDefault : Id -> String -> List Prop -> Move.Move Float
stateOrDefault targetId targetName props =
    case props of
        [] ->
            Internal.Css.Props.default targetId

        (Prop id name move _) :: remain ->
            if (targetId - Internal.Css.Props.noId) == 0 then
                if name == targetName then
                    move

                else
                    stateOrDefault targetId targetName remain

            else if id == targetId then
                move

            else
                stateOrDefault targetId targetName remain

        (ColorProp name movement clr) :: remain ->
            stateOrDefault targetId targetName remain


{-| -}
colorOrDefault : String -> Color.Color -> List Prop -> Color.Color
colorOrDefault targetName default props =
    case props of
        [] ->
            default

        (Prop id _ move _) :: remain ->
            colorOrDefault targetName default remain

        (ColorProp name movement clr) :: remain ->
            if targetName == name then
                clr

            else
                colorOrDefault name default remain


matchForMovement : Id -> String -> List Prop -> Maybe (Move.Move Float)
matchForMovement onlyId onlyName props =
    case props of
        [] ->
            Nothing

        (ColorProp name move clr) :: remain ->
            matchForMovement onlyId onlyName remain

        ((Prop id name movement _) as top) :: remain ->
            if id + 1 == 0 then
                if name == onlyName then
                    Just movement

                else
                    matchForMovement onlyId onlyName remain

            else if id - onlyId == 0 then
                Just movement

            else
                matchForMovement onlyId onlyName remain


{-| A group of curves represents the trail of one scalar property

    (Scalar property meaning something like opacity, or just the `R` channel of rgb.)

-}
type RenderedProp
    = RenderedProp RenderedPropDetails
    | CompoundProp Compound
    | RenderedColorProp RenderedColorPropDetails
    | TransformProp TransformPropDetails


type alias TransformPropDetails =
    { sections :
        List
            { delay : Duration.Duration
            , sequence : Move.Sequence Transform
            }
    , state : TransformState
    }


type alias TransformState =
    { x : Interpolate.State
    , y : Interpolate.State
    , scale : Interpolate.State
    , rotation : Interpolate.State
    }


type alias Transform =
    { x : Float
    , y : Float
    , scale : Float
    , rotation : Float
    }


type alias RenderedColorPropDetails =
    { name : String
    , color : Color.Color
    , sections :
        List
            { delay : Duration.Duration
            , sequence : Move.Sequence Color.Color
            }
    }


type alias RenderedPropDetails =
    { id : Id
    , name : String
    , format : Internal.Css.Props.Format
    , sections :
        List
            { delay : Duration.Duration
            , sequence : Move.Sequence Float
            }
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


type alias Compound =
    --         --> into the future
    { slices : List CompoundSection

    --         V-- across props
    , states : List ( Id, Interpolate.State )
    }


{-| A compound section represents all the
props transitioning from one state to another.

In CSS terms, each compound section is it's own
@keyframe definition

-}
type alias CompoundSection =
    { start : Time.Absolute
    , period : Timeline.Period
    , conflicting : Bool
    , frames : List Keyframe
    }


{-| A Keyframe are all the properties necesasry to render a single keyframe.
-}
type alias Keyframe =
    { start : Time.Absolute
    , end : Time.Absolute
    , timing : Bezier.Spline

    -- {1, spline}, {2, spline}
    -- If the splines are not conflicting
    , props :
        List
            { id : Id
            , spline : Bezier.Spline
            }
    }


{-| Taking a list of elements and slicing it so that it only contains frames before the checkpoint.

And renormalize the percents so they still make sense

-}
renormalizeBefore :
    Int
    ->
        List
            { percent : Int
            , color : Color.Color
            }
    ->
        List
            { percent : Int
            , color : Color.Color
            }
renormalizeBefore checkpoint steps =
    renormalizeBeforeHelper checkpoint steps []


renormalizeBeforeHelper checkpoint steps captured =
    case steps of
        [] ->
            captured

        step :: remain ->
            if step.percent - checkpoint < 0 then
                let
                    renormalized =
                        { percent =
                            (toFloat step.percent * (1 / toFloat checkpoint))
                                * 100
                                |> round
                        , color = step.color
                        }
                in
                renormalizeBeforeHelper
                    checkpoint
                    remain
                    (renormalized :: captured)

            else
                case remain of
                    [] ->
                        captured

                    final :: _ ->
                        let
                            progress =
                                toFloat (checkpoint - step.percent)
                                    / toFloat (final.percent - step.percent)
                        in
                        { percent = 100
                        , color = Interpolate.color progress step.color final.color
                        }
                            :: captured


renormalizeAfter :
    Int
    ->
        List
            { percent : Int
            , color : Color.Color
            }
    ->
        List
            { percent : Int
            , color : Color.Color
            }
renormalizeAfter checkpoint steps =
    renormalizeAfterHelper checkpoint steps []


renormalizeAfterHelper checkpoint steps captured =
    case steps of
        [] ->
            captured

        step :: remain ->
            if step.percent - checkpoint > 0 then
                let
                    renormalized =
                        { percent =
                            step.percent - checkpoint
                        , color = step.color
                        }
                in
                renormalizeAfterHelper
                    checkpoint
                    remain
                    (renormalized :: captured)

            else
                renormalizeAfterHelper
                    checkpoint
                    remain
                    captured


splitSection : Time.Absolute -> Section -> ( Section, Section, Maybe Section )
splitSection at (Section section) =
    let
        split =
            Bezier.splitList (Time.inMilliseconds at)
                section.splines
                []

        end =
            section.start
                |> Time.advanceBy (Timeline.periodDuration section.period)

        before =
            { start = section.start
            , period = once (Time.duration section.start at)
            , splines = split.before
            }

        after =
            { start = at
            , period = once (Time.duration at end)
            , splines = split.after
            }

        remaining =
            if isOnce section.period then
                Nothing

            else
                Just
                    (Section
                        { start = end
                        , period = Timeline.reduceIterations 1 section.period
                        , splines = section.splines
                        }
                    )
    in
    ( Section before
    , Section after
    , remaining
    )


{-| Splitting a section is useful when rendering because maybe `now` is in the middle of a section.

In the standard case, splitting a compound would return two sections.

However, in the case of a repeating or looping event, it may return three.

split here--|
v
A------------B(x3)

    Will result in

    |---1---|--2-|----3(x2)------|

-}
splitCompoundSection : Time.Absolute -> CompoundSection -> ( CompoundSection, CompoundSection, Maybe CompoundSection )
splitCompoundSection at cpd =
    let
        split =
            splitKeyframeList at
                cpd.frames

        beforeDuration =
            Time.duration cpd.start at

        before =
            { start = cpd.start
            , period = once beforeDuration
            , conflicting = cpd.conflicting
            , frames = split.before
            }

        duration =
            Timeline.periodDuration cpd.period

        after =
            { start = at
            , period = once (duration |> Quantity.minus beforeDuration)
            , conflicting = cpd.conflicting
            , frames = split.after
            }

        remaining =
            if isOnce cpd.period then
                Nothing

            else
                Just
                    { start =
                        cpd.start
                            |> Time.advanceBy (Timeline.periodDuration cpd.period)
                    , period = Timeline.reduceIterations 1 cpd.period
                    , conflicting = cpd.conflicting
                    , frames = cpd.frames
                    }
    in
    ( before
    , after
    , remaining
    )


splitKeyframeList : Time.Absolute -> List Keyframe -> { before : List Keyframe, after : List Keyframe }
splitKeyframeList at frames =
    splitKeyframeListHelper at frames []


splitKeyframeListHelper :
    Time.Absolute
    -> List Keyframe
    -> List Keyframe
    ->
        { before : List Keyframe
        , after : List Keyframe
        }
splitKeyframeListHelper now frames passed =
    case frames of
        [] ->
            { before = List.reverse passed
            , after = []
            }

        top :: remain ->
            if Time.equal now top.start then
                { before = List.reverse passed
                , after = frames
                }

            else if Time.thisBeforeThat now top.end then
                -- first frame that
                let
                    split =
                        splitKeyframe now top
                in
                { before = List.reverse (split.before :: passed)
                , after = split.after :: remain
                }

            else
                splitKeyframeListHelper now
                    remain
                    (top :: passed)


splitKeyframe : Time.Absolute -> Keyframe -> { before : Keyframe, after : Keyframe }
splitKeyframe now keyframe =
    let
        nowMS =
            Time.inMilliseconds now

        ( beforeTiming, afterTiming ) =
            Bezier.splitAtX nowMS keyframe.timing

        propsBefore =
            List.map
                (\prop ->
                    { id = prop.id
                    , spline = Tuple.first (Bezier.splitAtX nowMS prop.spline)
                    }
                )
                keyframe.props

        propsAfter =
            List.map
                (\prop ->
                    { id = prop.id
                    , spline =
                        Tuple.second (Bezier.splitAtX nowMS prop.spline)
                    }
                )
                keyframe.props
    in
    { before =
        { start = keyframe.start
        , end = now
        , timing = beforeTiming
        , props = propsBefore
        }
    , after =
        { start = now
        , end = keyframe.end
        , timing = afterTiming
        , props = propsAfter
        }
    }


{-| -}
type alias CompoundFrame =
    { -- A compound section is only valid if the only moving props share a personality
      -- ultimately we're going to need every prop for every keyframe
      props :
        --         V-- across props
        List
            { id : Id
            , movement : CapturedMovement
            }
    }


type CapturedMovement
    = BySpline (List Bezier.Spline)
    | Stationary Float


{-| -}
lerpCurvesCompoundHelper2 :
    List ( Id, Interpolate.State )
    -> (state -> List Prop)
    -> Timeline.Occurring state
    -> Timeline.Occurring state
    -> Time.Absolute
    -> List (Timeline.Occurring state)
    -> Bool
    -> List Keyframe
    -> List ( Id, Interpolate.State )
    ->
        { keyframes : List Keyframe
        , conflicting : Bool
        , states : List ( Id, Interpolate.State )
        }
lerpCurvesCompoundHelper2 remainingStates lookup prev target now future conflicted keyframes updatedStates =
    case remainingStates of
        [] ->
            { states = List.reverse updatedStates
            , keyframes = keyframes
            , conflicting = False
            }

        ( id, state ) :: remain ->
            let
                startTime =
                    Timeline.endTime prev

                targetTime =
                    Timeline.startTime target

                prevState =
                    lookupState (Timeline.getEvent prev)

                targetState =
                    lookupState (Timeline.getEvent target)

                lookupState s =
                    stateOrDefault id "" (lookup s)

                progress =
                    Time.progress startTime targetTime now

                targetPosition =
                    case targetState of
                        Move.Pos _ x _ ->
                            Pixels.pixels x

                targetTransition =
                    case targetState of
                        Move.Pos trans _ _ ->
                            trans

                domain =
                    { start =
                        { x = startTime
                        , y =
                            --state.position
                            case prevState of
                                Move.Pos _ x _ ->
                                    Pixels.pixels x
                        }
                    , end =
                        { x = targetTime
                        , y = targetPosition
                        }
                    }

                targetVelocity =
                    --Interpolate.velocityAtTarget lookupState target future
                    Pixels.pixelsPerSecond 0

                newState =
                    Transition.atX
                        progress
                        domain
                        state.velocity
                        targetVelocity
                        targetTransition

                splines =
                    Transition.splines
                        domain
                        state.velocity
                        targetVelocity
                        targetTransition

                new =
                    case keyframes of
                        [] ->
                            { keyframes =
                                List.map
                                    (splineToKeyframe id)
                                    splines
                            , conflicting = conflicted
                            }

                        _ ->
                            mergeIntoKeyframes conflicted
                                id
                                splines
                                []
                                keyframes
            in
            lerpCurvesCompoundHelper2
                remain
                lookup
                prev
                target
                now
                future
                new.conflicting
                new.keyframes
                (( id, newState ) :: updatedStates)


{-| We know that all splines here cover the same domain, because they are simply a transition
from state A to state B.

The most common case is that each list of beziers is exactly one spline long.

However for springs, there may be a sequence of beziers

    A B

1 |--------------| (normal)
2 |------|--|--|-| (spring)

In this case we know that they start at the same point (A).

When we get a (1) first, we should create a single keyframe and add it

ON the second iteration, if (2) shows up, we should split (1) into sections so it matches (2), and then merge.

-}
mergeIntoKeyframes : Bool -> Id -> List Bezier.Spline -> List Keyframe -> List Keyframe -> { conflicting : Bool, keyframes : List Keyframe }
mergeIntoKeyframes conflicting id splines newKeyframes keyframes =
    case splines of
        [] ->
            { conflicting = conflicting
            , keyframes = newKeyframes
            }

        topSpline :: remainSpline ->
            case keyframes of
                [] ->
                    -- This isnt quite right
                    -- However, I don't think should ever happen :thinking:
                    { conflicting = conflicting
                    , keyframes =
                        newKeyframes
                            ++ List.map
                                (splineToKeyframe id)
                                splines
                    }

                keyTop :: remainingKeyframes ->
                    -- we know that the starts of topSpline and keyTop match
                    -- because that's one of the things we're maintaining.
                    if Time.inMilliseconds keyTop.end == Bezier.lastX topSpline then
                        -- exact match, add ths prop to this keyframe
                        let
                            -- this section is conflicting if
                            --    1. it was already marked as conflicting
                            --    2. topSpline moves
                            sectionIsConflicting =
                                conflicting
                                    || (not (Bezier.doesNotMove topSpline)
                                            && not (Bezier.doesNotMove keyTop.timing)
                                       )

                            new =
                                { keyTop
                                    | props =
                                        { id = id, spline = topSpline } :: keyTop.props
                                    , timing =
                                        if Bezier.doesNotMove keyTop.timing && not (Bezier.doesNotMove topSpline) then
                                            -- *NOTE* convert to timing bezier
                                            topSpline

                                        else
                                            keyTop.timing
                                }
                                    :: newKeyframes
                        in
                        mergeIntoKeyframes sectionIsConflicting id remainSpline new remainingKeyframes

                    else if Time.inMilliseconds keyTop.end < Bezier.lastX topSpline then
                        -- If `topSpline is bigger than keyTop
                        --   Chop off the front of topSpline and add it to keyTop
                        --   Iterate with the second half of topSpline and splineRemain
                        let
                            new =
                                keyTop :: newKeyframes

                            -- TODO: Note, there is a case where these are *not* conflicting
                            -- which is when there is only one prop (which is a spring), which is moving.
                        in
                        mergeIntoKeyframes True id remainSpline new remainingKeyframes

                    else
                        -- topSpline is shorter than keyTop
                        --    Chop keyTop into 2 keyframes
                        --    Add topSpline to slive[1] and add to `new`
                        --    Continue on, maintaining slice[1] in remainingKeyframes
                        let
                            new =
                                keyTop :: newKeyframes

                            -- TODO: Note, there is a case where these are *not* conflicting
                            -- which is when there is only one prop (which is a spring), which is moving.
                        in
                        mergeIntoKeyframes True id remainSpline new remainingKeyframes


splineToKeyframe :
    Id
    -> Bezier.Spline
    -> Keyframe
splineToKeyframe id spline =
    { start = Time.millis (Bezier.firstX spline)
    , end = Time.millis (Bezier.lastX spline)

    -- NOTE: This should be normalized to a time-spline
    , timing = spline
    , props =
        [ { id = id
          , spline = spline
          }
        ]
    }


isCombineableCompoundSections : CompoundSection -> CompoundSection -> Bool
isCombineableCompoundSections one two =
    (one.conflicting == two.conflicting)
        && (isOnce one.period && isOnce two.period)


isOnce : Timeline.Period -> Bool
isOnce period =
    case period of
        Timeline.Repeat 1 _ ->
            True

        _ ->
            False


periodDuration : Timeline.Period -> Time.Duration
periodDuration period =
    case period of
        Timeline.Repeat _ dur ->
            dur

        Timeline.Loop dur ->
            dur


combineCompound : CompoundSection -> CompoundSection -> CompoundSection
combineCompound one two =
    { start = Time.earliest one.start two.start
    , period =
        once
            (Time.expand
                (periodDuration one.period)
                (periodDuration two.period)
            )
    , conflicting = one.conflicting
    , frames = two.frames ++ one.frames
    }


once : Time.Duration -> Timeline.Period
once =
    Timeline.Repeat 1


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
    -> (state -> Move.Move Float)
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
            , props = []
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
                    String.fromInt
                        (round
                            (Duration.inMilliseconds (Time.duration stack.stackStart stack.stackEnd))
                        )
                        ++ "ms"

                delay =
                    Time.duration now stack.stackStart
                        |> Duration.inMilliseconds
                        |> round
                        |> String.fromInt
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
                        ++ " 1 normal forwards running "
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
            , props = []
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
    , props = []
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
    -> (state -> Move.Move Float)
    ->
        Timeline.Interp state
            (Move.Move Float)
            { css : CssAnim
            , stackStart : Time.Absolute
            , stackEnd : Time.Absolute
            , stack : List Interpolate.Checkpoint
            , state : Interpolate.State
            }
toCss now name renderValue toMotion =
    { start =
        \motion ->
            { css = emptyAnim
            , stackStart = now
            , stackEnd = now
            , stack = []
            , state = Interpolate.moving.start motion
            }
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
                    -- previousl we'd add a dwell here
                    -- addDwell lookup
                    --     name
                    --     renderValue
                    --     target
                    --     targetTime
                    --     now
                    --     state
                    --     { css =
                    --         final
                    --             |> finalize name renderValue now
                    --             |> combine data.css
                    --     , stackStart = targetTime
                    --     , stackEnd = targetTime
                    --     , stack = []
                    --     , state = state
                    --     }
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
    , transition =
        \prevEndTime prev target targetTime now_IGNORE maybeLookAhead data ->
            -- finalize current stack
            -- create and finalizeTransition stack for the interruption
            -- (but use the special transition finalizer which embeds timing outside of keyframes)
            let
                transitionSplines =
                    Interpolate.transitionSplines
                        prevEndTime
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
                Interpolate.moving.transition
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
                    String.fromInt (round (top.time * 100)) ++ "%"

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

        Interpolate.Bezier spline ->
            Bezier.hash spline


encodeFloat : Float -> String
encodeFloat fl =
    String.fromInt (round fl)


renderTiming : Interpolate.Timing -> String
renderTiming timing =
    case timing of
        Interpolate.Linear ->
            "linear"

        Interpolate.Bezier spline ->
            -- the spline passed here needs to be normalized over 0-1
            -- and then we only need to pass the two control points to the css animation
            Bezier.cssTimingString spline


{-| Slightly different than CssAnim in that we can also have style properties
This is for when a property has not changed and so a full animation is not necessary
-}
type alias CssAnim =
    { hash : String

    -- use single prop encoding:
    -- https://developer.mozilla.org/en-US/docs/Web/CSS/animation
    , animation : String
    , keyframes : String

    -- These are generally used as backups
    -- its possible a prop wont be animated this transition,
    -- so it's easy enough to just say "background-color" "blue" in that case
    , props : List ( String, String )
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
