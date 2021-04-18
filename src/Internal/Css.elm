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
import Internal.Transition as Transition
import Pixels
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
      Prop Id String Interpolate.Movement Internal.Css.Props.Format
      -- | ColorProp ColorPropDetails
    | ColorProp String Interpolate.Movement Color.Color


applyToMovement : (Interpolate.Movement -> Interpolate.Movement) -> Prop -> Prop
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
                if Set.member id ids then
                    add remain existingProps names ids

                else
                    add remain
                        (prop :: existingProps)
                        names
                        (Set.insert id ids)

            else if Set.member name names then
                add remain existingProps names ids

            else
                add remain
                    (prop :: existingProps)
                    (Set.insert name names)
                    ids

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


scanProps :
    Timeline.Interp state
        (List Prop)
        { props : List Prop
        , cache :
            { name : Set String
            , id : Set Id
            }
        }
scanProps =
    { start =
        \props ->
            add props [] Set.empty Set.empty
    , visit =
        \lookup target targetTime maybeLookAhead data ->
            add (lookup (Timeline.getEvent target))
                data.props
                data.cache.name
                data.cache.id
    , transition =
        \prevEndTime prev target targetTime interruptedAt maybeLookAhead data ->
            add target data.props data.cache.name data.cache.id
    }


cssFromProps : Timeline.Timeline state -> (state -> List Prop) -> CssAnim
cssFromProps timeline lookup =
    let
        present =
            Timeline.foldpAll lookup scanProps timeline
                |> .props
                |> List.sortBy propOrder

        renderedProps =
            -- Timeline.foldpAll lookup (toPropCurves present) timeline
            --     |> .rendered
            -- |> Debug.log "RENDERED"
            Timeline.foldpAll2 lookup (startProps present) (toPropCurves2 present) timeline
    in
    renderCss (Timeline.getCurrentTime timeline) renderers renderedProps


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
    case renderedProps of
        [] ->
            Nothing

        (RenderedColorProp details) :: remain ->
            let
                rendered =
                    renderColorSection now details.name details.sections emptyAnim
            in
            case colors now remain of
                Nothing ->
                    Just ( rendered, remain )

                Just ( renderedColors, left ) ->
                    Just
                        ( combine renderedColors rendered
                        , left
                        )

        _ ->
            Nothing


{-| -}
renderColorSection : Time.Absolute -> String -> List ColorSection -> CssAnim -> CssAnim
renderColorSection now name sections anim =
    case sections of
        [] ->
            anim

        top :: remain ->
            let
                end =
                    top.start
                        |> Time.advanceBy top.duration
            in
            if Time.thisBeforeThat end now then
                renderColorSection
                    now
                    name
                    remain
                    anim

            else if Time.thisBeforeThat top.start now && Time.thisAfterThat end now then
                case splitColorSection now top of
                    ( before_, after ) ->
                        renderColorSection
                            now
                            name
                            remain
                            (combine anim
                                (renderColorSectionToCss now name after)
                            )

            else
                renderColorSection
                    now
                    name
                    remain
                    (combine anim
                        (renderColorSectionToCss now name top)
                    )


renderColorSectionToCss : Time.Absolute -> String -> ColorSection -> CssAnim
renderColorSectionToCss now name section =
    let
        animationName =
            name
                ++ "-"
                ++ multiColorHash section.steps ""

        durationStr =
            (Duration.inMilliseconds section.duration
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
            "1"

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
                ++ colorFrames name section.steps ""
                ++ "\n}"
    in
    { hash = animationName
    , animation = animation
    , keyframes = keyframes
    }


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
                cssAnim =
                    renderCompoundSections now comp.slices emptyAnim
            in
            Just ( cssAnim, remain )

        _ ->
            Nothing


renderCompoundSections : Time.Absolute -> List CompoundSection -> CssAnim -> CssAnim
renderCompoundSections now sections anim =
    case Debug.log "SECTIONS" sections of
        [] ->
            anim

        targetSection :: tail ->
            let
                -- _ =
                --     Debug.log "target" targetSection
                end =
                    targetSection.start
                        |> Time.advanceBy (Timeline.periodDuration targetSection.period)
                        |> Debug.log "END CALCULATED AS"

                onlyOnce =
                    Timeline.isOnce targetSection.period
            in
            if onlyOnce && Time.thisBeforeThat end now then
                -- Debug.log "ONLY ONCE"
                let
                    _ =
                        Debug.log "  -> passed one section" tail
                in
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
                            Debug.log "SPLITTING SECTIONS"
                                ( active
                                , case maybeNext of
                                    Nothing ->
                                        tail

                                    Just next ->
                                        next :: tail
                                )

                        else
                            ( targetSection, tail )

                    _ =
                        Debug.log "FRAMES" section.frames

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

                    _ =
                        Debug.log "DELAY" delay

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
                _ =
                    Debug.log "COMPOUND FRAME"
                        { progress = percentage
                        , duration = Timeline.periodDuration period
                        , start = start
                        , end = start |> Time.advanceBy (Timeline.periodDuration period)
                        }

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
                (propToCssHelper now details.id details.name details.format details.sections emptyAnim
                    |> combine anim
                )

        (CompoundProp _) :: remain ->
            -- handled by transform renderer
            scalarHelper now remain anim

        (RenderedColorProp _) :: remain ->
            -- handled by color renderer
            scalarHelper now remain anim


propToCssHelper : Time.Absolute -> Internal.Css.Props.Id -> String -> Internal.Css.Props.Format -> List Section -> CssAnim -> CssAnim
propToCssHelper now id name format sections anim =
    case sections of
        [] ->
            anim

        top :: remain ->
            propToCssHelper now
                id
                name
                format
                remain
                (combine
                    anim
                    (sectionCss now id name format top)
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
            let
                _ =
                    Debug.log "Rendering"
                        { splines = splines
                        , f = f
                        }
            in
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

        _ =
            if name == "border-width" then
                -- Debug.log "BORDER"
                { hash = animationName
                , animation = animation
                , keyframes = keyframes
                }

            else
                { hash = animationName
                , animation = animation
                , keyframes = keyframes
                }
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
                    rendered

                Just cmpd ->
                    CompoundProp cmpd
                        :: rendered

        (ColorProp name movement clr) :: remain ->
            rendered

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


oneShot :
    Time.Absolute
    ->
        Timeline.Interp state
            (List Prop)
            { css : CssAnim
            , states : List ( String, Interpolate.State )
            , previous : Maybe (Timeline.Occurring state)
            }
oneShot now =
    let
        _ =
            Debug.log "now" now
    in
    { start =
        \props ->
            { css = emptyAnim
            , states =
                props
                    |> List.sortBy propOrder
                    |> List.map
                        (\p ->
                            case p of
                                Prop id name m format ->
                                    ( name, Interpolate.moving.start m )

                                ColorProp name m clr ->
                                    ( name, Interpolate.moving.start m )
                        )
            , previous = Nothing
            }
    , visit =
        \lookup target targetTime maybeLookAhead data ->
            let
                _ =
                    Debug.log "ONE - VISIT" target

                {-
                    domain =
                        { start =
                            { x = Time.inMilliseconds prevEndTime
                            , y = Pixels.inPixels state.position
                            }
                        , end =
                            { x = Time.inMilliseconds targetTime
                            , y = targetPos
                            }
                        }

                   scalars ->
                       Transition.keyframes domain introVelocity targetVelocity (\f -> propName ++ format f) transition
                       Transition.hash start startVelocity end endVelocity transition
                       animation?

                    colors ->
                        Same as scalars, but target toggles between 0 and 1 and renders that value to a different color depending.

                    transform ->
                        Transition.compoundKeyframes :
                            (\transforms -> "transform: " ++ String.join " " transforms)
                            -> List ( Float -> String, Transition ) -> String
                        Transition.compoundHash : List ( Float -> String, Transition ) -> String
                        animation?


                -}
                new =
                    List.foldl
                        (\( id, state ) cursor ->
                            -- let
                            --     targetPos =
                            --         case target of
                            --             Pos _ x _ ->
                            --                 x
                            --     targetVelocity =
                            --         newVelocityAtTarget target targetTime maybeLookAhead
                            --             |> Pixels.inPixelsPerSecond
                            -- in
                            -- Transition.splines
                            -- { start =
                            --     { x = Time.inMilliseconds prevEndTime
                            --     , y = Pixels.inPixels state.position
                            --     }
                            -- , end =
                            --     { x = Time.inMilliseconds targetTime
                            --     , y = targetPos
                            --     }
                            -- }
                            --     (Pixels.inPixelsPerSecond state.velocity * 1000)
                            --     (targetVelocity * 1000)
                            --     (case target of
                            --         Pos trans _ _ ->
                            --             trans
                            --     )
                            let
                                propLookup s =
                                    lookup s
                                        |> stateOrDefaultByName id

                                maybePropLookAhead =
                                    Maybe.map
                                        (Timeline.mapLookAhead
                                            (stateOrDefaultByName id)
                                        )
                                        maybeLookAhead

                                -- targetProp =
                                --     stateOrDefaultByName id (lookup target)
                                -- targetTransition =
                                --     case targetProp of
                                --         Prop propId name (Interpolate.Pos t v seq) format ->
                                --             t
                                --         ColorProp name (Interpolate.Pos t v seq) clr ->
                                --             t
                                -- targetPos =
                                --     case targetProp of
                                --         Prop propId name (Interpolate.Pos t v seq) format ->
                                --             v
                                --         ColorProp name (Interpolate.Pos t v seq) clr ->
                                --             v
                                -- prevEndTime =
                                --     case data.previous of
                                --         Nothing ->
                                --             targetTime
                                --         Just prevEvent ->
                                --             Timeline.endTime prevEvent
                                -- domain =
                                --     { start =
                                --         { x = Time.inMilliseconds prevEndTime
                                --         , y = Pixels.inPixels state.position
                                --         }
                                --     , end =
                                --         { x = Time.inMilliseconds targetTime
                                --         , y = targetPos
                                --         }
                                --     }
                                -- intro =
                                --     0
                                -- exit =
                                --     0
                                -- toPropString f =
                                --     case targetProp of
                                --         Prop propId name (Interpolate.Pos t v seq) format ->
                                --             name ++ ":" ++ Internal.Css.Props.format format f
                                --         ColorProp name (Interpolate.Pos t v seq) clr ->
                                --             -- TODO: this needs to be lerped using `f` as the t
                                --             -- and the previous value as the starting point
                                --             name ++ ":" ++ Color.toCssString clr
                                -- keys =
                                --     Transition.keyframes domain intro exit toPropString targetTransition
                            in
                            { states =
                                ( id
                                , Interpolate.moving.visit
                                    propLookup
                                    target
                                    targetTime
                                    maybePropLookAhead
                                    state
                                )
                                    :: cursor.states
                            }
                        )
                        { states = []
                        }
                        data.states
            in
            { css = emptyAnim
            , states =
                List.reverse new.states
            , previous = Just target
            }
    , transition =
        \prevEndTime prev target targetTime interruptedAt maybeLookAhead data ->
            let
                _ =
                    Debug.log "ONE - TRANS" ( targetTime, target )
            in
            { css = emptyAnim
            , states =
                List.map
                    (\( id, state ) ->
                        --     splines =
                        -- Interpolate.transitionSplines
                        --     interruptedAt
                        --     prevState
                        --     targetState
                        --     targetTime
                        --     lookAheadState
                        --     state
                        -- transitionSplines prevEndTime prev target targetTime maybeLookAhead state =
                        --     let
                        --         targetPos =
                        --             case target of
                        --                 Pos _ x _ ->
                        --                     x
                        --         targetVelocity =
                        --             newVelocityAtTarget target targetTime maybeLookAhead
                        --                 |> Pixels.inPixelsPerSecond
                        --     in
                        --     Transition.splines
                        --         { start =
                        --             { x = Time.inMilliseconds prevEndTime
                        --             , y = Pixels.inPixels state.position
                        --             }
                        --         , end =
                        --             { x = Time.inMilliseconds targetTime
                        --             , y = targetPos
                        --             }
                        --         }
                        --         (Pixels.inPixelsPerSecond state.velocity * 1000)
                        --         (targetVelocity * 1000)
                        --         (case target of
                        --             Pos trans _ _ ->
                        --                 trans
                        --         )
                        -- new =
                        -- case keyframes of
                        --     [] ->
                        --         { keyframes =
                        --             List.map
                        --                 (splineToKeyframe id)
                        --                 splines
                        --         , conflicting = conflicted
                        --         }
                        --     _ ->
                        --         mergeIntoKeyframes conflicted
                        --             id
                        --             splines
                        --             []
                        --             keyframes
                        let
                            previousProp =
                                stateOrDefaultByName id prev

                            targetProp =
                                stateOrDefaultByName id target

                            lookAheadProp =
                                Maybe.map
                                    (Timeline.mapLookAhead
                                        (stateOrDefaultByName id)
                                    )
                                    maybeLookAhead
                        in
                        ( id
                        , Interpolate.moving.transition
                            prevEndTime
                            previousProp
                            targetProp
                            targetTime
                            interruptedAt
                            lookAheadProp
                            state
                        )
                    )
                    data.states
            , previous = Nothing
            }
    }


{-| -}
toPropCurves2 : List Prop -> Timeline.Transition state (List Prop) (List RenderedProp)
toPropCurves2 only =
    \lookup prev target now future cursor ->
        let
            _ =
                Debug.log "TRANSITION" target

            targetTime =
                Timeline.startTime target

            startTime =
                Timeline.endTime prev
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

                            newColor =
                                colorOrDefault details.name
                                    Internal.Css.Props.transparent
                                    (lookup (Timeline.getEvent target))
                        in
                        RenderedColorProp
                            { details
                                | color = newColor
                                , sections =
                                    { start = targetTime
                                    , duration =
                                        Time.duration startTime targetTime
                                    , steps =
                                        [ { percent = 0
                                          , color = previousColor
                                          }
                                        , { percent = 50
                                          , color = Interpolate.color 0.5 previousColor newColor
                                          }
                                        , { percent = 100
                                          , color = newColor
                                          }
                                        ]
                                    }
                                        :: details.sections
                            }

                    RenderedProp rendered ->
                        let
                            propLookup : Timeline.Occurring state -> Interpolate.Movement
                            propLookup occur =
                                stateOrDefault rendered.id rendered.name (lookup (Timeline.getEvent occur))

                            lookupState s =
                                stateOrDefault rendered.id rendered.name (lookup s)

                            targetProp =
                                propLookup target

                            domain =
                                { start =
                                    { x = startTime
                                    , y = rendered.state.position
                                    }
                                , end =
                                    { x = targetTime
                                    , y = targetPosition
                                    }
                                }

                            targetVelocity =
                                Interpolate.velocityAtTarget lookupState target future

                            progress =
                                Time.progress startTime targetTime now
                                    |> Debug.log "PROGRESS"

                            targetPosition =
                                case targetProp of
                                    Interpolate.Pos _ x _ ->
                                        Pixels.pixels x

                            targetTransition =
                                case targetProp of
                                    Interpolate.Pos trans _ _ ->
                                        trans
                        in
                        RenderedProp
                            { id = rendered.id
                            , name = rendered.name
                            , format = rendered.format
                            , sections =
                                let
                                    transitionSplines =
                                        Transition.splines
                                            domain
                                            rendered.state.velocity
                                            targetVelocity
                                            targetTransition

                                    dur =
                                        Time.duration startTime targetTime

                                    sliced =
                                        -- if interruptedAt == targetTime then
                                        -- NOTE, this needs to be checked!!
                                        if Time.thisBeforeThat now targetTime && Time.thisAfterThat now startTime then
                                            Bezier.takeBefore (Time.inMilliseconds now) transitionSplines

                                        else
                                            transitionSplines
                                in
                                Section
                                    { start = startTime
                                    , period = once dur
                                    , splines = sliced
                                    }
                                    :: rendered.sections
                            , state =
                                Transition.atX
                                    progress
                                    domain
                                    rendered.state.velocity
                                    targetVelocity
                                    targetTransition
                            }

                    CompoundProp details ->
                        -- let
                        --     ( newCompound, newStates ) =
                        --         lerpCurvesCompound
                        --             details.states
                        --             prevEndTime
                        --             prev
                        --             target
                        --             targetTime
                        --             interruptedAt
                        --             maybeLookAhead
                        -- in
                        -- CompoundProp
                        --     { slices =
                        --         case details.slices of
                        --             [] ->
                        --                 [ newCompound ]
                        --             last :: remain ->
                        --                 if isCombineableCompoundSections newCompound last then
                        --                     combineCompound newCompound last :: remain
                        --                 else
                        --                     newCompound :: details.slices
                        --     , states = newStates
                        --     }
                        prop
            )
            cursor


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
                startProps only props
            , previous = Nothing
            }
    , visit =
        \lookup target targetTime maybeLookAhead data ->
            let
                _ =
                    Debug.log "VISIT" target
            in
            { rendered =
                case data.previous of
                    Nothing ->
                        data.rendered

                    Just prev ->
                        List.map
                            (\prop ->
                                case prop of
                                    RenderedColorProp details ->
                                        let
                                            previousTime =
                                                case data.previous of
                                                    Nothing ->
                                                        targetTime

                                                    Just prevEvent ->
                                                        Timeline.endTime prevEvent

                                            newColor =
                                                lookup (Timeline.getEvent target)
                                                    |> colorOrDefault details.name
                                                        Internal.Css.Props.transparent

                                            new =
                                                { details
                                                    | color = newColor
                                                    , sections =
                                                        { start = targetTime
                                                        , duration =
                                                            Time.duration previousTime targetTime
                                                        , steps =
                                                            [ { percent = 0
                                                              , color = details.color
                                                              }
                                                            , { percent = 50
                                                              , color = Interpolate.color 0.5 details.color newColor
                                                              }
                                                            , { percent = 100
                                                              , color = newColor
                                                              }
                                                            ]
                                                        }
                                                            :: details.sections
                                                }
                                        in
                                        RenderedColorProp new

                                    RenderedProp rendered ->
                                        let
                                            propLookup state =
                                                let
                                                    found =
                                                        lookup state
                                                            |> stateOrDefault rendered.id rendered.name

                                                    -- _ =
                                                    --     Debug.log "    ->"
                                                    --         { rendered = rendered
                                                    --         , found = found
                                                    --         -- lookup target
                                                    --         --     |> stateOrDefault rendered.id rendered.name
                                                    --         -- propLookup target
                                                    --         }
                                                in
                                                found

                                            maybePropLookAhead =
                                                Maybe.map
                                                    (Timeline.mapLookAhead
                                                        (stateOrDefault rendered.id rendered.name)
                                                    )
                                                    maybeLookAhead
                                        in
                                        RenderedProp
                                            { id = rendered.id
                                            , name = rendered.name
                                            , format = rendered.format
                                            , sections =
                                                toCurvesVisit
                                                    propLookup
                                                    target
                                                    targetTime
                                                    data.previous
                                                    maybePropLookAhead
                                                    rendered.state
                                                    rendered.sections
                                            , state =
                                                Interpolate.moving.visit
                                                    propLookup
                                                    target
                                                    targetTime
                                                    maybePropLookAhead
                                                    rendered.state
                                            }

                                    CompoundProp details ->
                                        let
                                            ( newCompound, newStates ) =
                                                visitCurvesCompound
                                                    details.states
                                                    lookup
                                                    prev
                                                    target
                                                    targetTime
                                                    maybeLookAhead
                                                    |> Debug.log "VISITED COMPOUND"
                                        in
                                        CompoundProp
                                            { slices =
                                                case details.slices of
                                                    [] ->
                                                        [ newCompound ]

                                                    last :: remain ->
                                                        if isCombineableCompoundSections newCompound last then
                                                            combineCompound newCompound last :: remain

                                                        else
                                                            newCompound :: details.slices
                                            , states = newStates
                                            }
                            )
                            data.rendered
            , previous = Just target
            }
    , transition =
        \prevEndTime prev target targetTime interruptedAt maybeLookAhead data ->
            let
                _ =
                    Debug.log "TRANSITION" target
            in
            { rendered =
                List.map
                    (\prop ->
                        case prop of
                            RenderedColorProp details ->
                                let
                                    previousColor =
                                        colorOrDefault details.name
                                            Internal.Css.Props.transparent
                                            prev

                                    newColor =
                                        colorOrDefault details.name
                                            Internal.Css.Props.transparent
                                            target

                                    new =
                                        { details
                                            | color = newColor
                                            , sections =
                                                { start = targetTime
                                                , duration =
                                                    Time.duration prevEndTime targetTime
                                                , steps =
                                                    [ { percent = 0
                                                      , color = previousColor
                                                      }
                                                    , { percent = 50
                                                      , color = Interpolate.color 0.5 previousColor newColor
                                                      }
                                                    , { percent = 100
                                                      , color = newColor
                                                      }
                                                    ]
                                                }
                                                    :: details.sections
                                        }
                                in
                                RenderedColorProp new

                            RenderedProp rendered ->
                                let
                                    -- _ =
                                    --     Debug.log "    ->"
                                    --         { rendered = rendered.name
                                    --         , found = Interpolate.base targetProp
                                    --         -- lookup target
                                    --         --     |> stateOrDefault rendered.id rendered.name
                                    --         -- propLookup target
                                    --         }
                                    previousProp =
                                        stateOrDefault rendered.id rendered.name prev

                                    targetProp =
                                        stateOrDefault rendered.id rendered.name target

                                    lookAheadProp =
                                        Maybe.map
                                            (Timeline.mapLookAhead
                                                (stateOrDefault rendered.id rendered.name)
                                            )
                                            maybeLookAhead
                                in
                                RenderedProp
                                    { id = rendered.id
                                    , name = rendered.name
                                    , format = rendered.format
                                    , sections =
                                        toCurvesLerp
                                            prevEndTime
                                            targetProp
                                            targetTime
                                            interruptedAt
                                            lookAheadProp
                                            rendered.state
                                            rendered.sections
                                    , state =
                                        Interpolate.moving.transition
                                            prevEndTime
                                            previousProp
                                            targetProp
                                            targetTime
                                            interruptedAt
                                            lookAheadProp
                                            (Debug.log "    INCOMING" rendered.state)
                                            |> Debug.log "     NEW STATE"
                                    }

                            CompoundProp details ->
                                let
                                    ( newCompound, newStates ) =
                                        lerpCurvesCompound
                                            details.states
                                            prevEndTime
                                            prev
                                            target
                                            targetTime
                                            interruptedAt
                                            maybeLookAhead
                                in
                                CompoundProp
                                    { slices =
                                        case details.slices of
                                            [] ->
                                                [ newCompound ]

                                            last :: remain ->
                                                if isCombineableCompoundSections newCompound last then
                                                    combineCompound newCompound last :: remain

                                                else
                                                    newCompound :: details.slices
                                    , states = newStates
                                    }
                    )
                    data.rendered
            , previous = Nothing
            }
    }


{-| -}
stateOrDefaultByName : String -> List Prop -> Interpolate.Movement
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


{-| -}
stateOrDefault : Id -> String -> List Prop -> Interpolate.Movement
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


matchForMovement : Id -> String -> List Prop -> Maybe Interpolate.Movement
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


type alias RenderedColorPropDetails =
    { name : String
    , color : Color.Color
    , sections : List ColorSection
    }


type alias ColorSection =
    { start : Time.Absolute
    , duration : Time.Duration
    , steps :
        List
            { percent : Int
            , color : Color.Color
            }
    }


type alias RenderedPropDetails =
    { id : Id
    , name : String
    , format : Internal.Css.Props.Format
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


type alias Compound =
    --         --> into the future
    { slices : List CompoundSection

    --         V-- across props
    , states : List ( Id, Interpolate.State )
    }


splitColorSection : Time.Absolute -> ColorSection -> ( ColorSection, ColorSection )
splitColorSection at colorSection =
    let
        end =
            colorSection.start
                |> Time.advanceBy colorSection.duration

        atPercent =
            round (Time.progress colorSection.start end at * 100)

        before =
            { start = colorSection.start
            , duration = Time.duration colorSection.start at
            , steps =
                colorSection.steps
                    |> renormalizeBefore atPercent
            }

        after =
            { start = at
            , duration = Time.duration at end
            , steps =
                colorSection.steps
                    |> renormalizeAfter atPercent
            }
    in
    ( before
    , after
    )


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

        before =
            { start = cpd.start
            , period = once (Time.duration cpd.start at)
            , conflicting = cpd.conflicting
            , frames = split.before
            }

        after =
            { start = cpd.start
            , period = once (Time.duration cpd.start at)
            , conflicting = cpd.conflicting
            , frames = split.before
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
                    , spline = Tuple.first (Bezier.splitAt nowMS prop.spline)
                    }
                )
                keyframe.props

        propsAfter =
            List.map
                (\prop ->
                    { id = prop.id
                    , spline = Tuple.second (Bezier.splitAt nowMS prop.spline)
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
    , transition =
        \prevEndTime prev target targetTime interruptedAt maybeLookAhead data ->
            { curves =
                toCurvesLerp
                    prevEndTime
                    target
                    targetTime
                    interruptedAt
                    maybeLookAhead
                    data.state
                    data.curves
            , previous = Nothing
            , state =
                Interpolate.moving.transition
                    prevEndTime
                    prev
                    target
                    targetTime
                    interruptedAt
                    maybeLookAhead
                    data.state
            }
    }


lerpCurvesCompound :
    List ( Id, Interpolate.State )
    -- previous end time
    -> Time.Absolute
    -- prev
    -> List Prop
    -- target
    -> List Prop
    -- target time
    -> Time.Absolute
    -- interrupted at
    -> Time.Absolute
    -> Maybe (Timeline.LookAhead (List Prop))
    -> ( CompoundSection, List ( Id, Interpolate.State ) )
lerpCurvesCompound states prevEndTime previous target targetTime interruptedAt maybeLookahead =
    let
        -- _ =
        --     Debug.log "LERP" ( prevEndTime, interruptedAt, targetTime )
        new =
            lerpCurvesCompoundHelper
                states
                prevEndTime
                previous
                target
                targetTime
                interruptedAt
                maybeLookahead
                False
                []
                []
    in
    ( { start = prevEndTime
      , period = once (Time.duration prevEndTime targetTime)
      , conflicting = new.conflicting
      , frames = new.keyframes
      }
    , new.states
    )



{-
   We have

       List
           { id : Id
           , movement : CapturedMovement: (List Spline)
           }


   We want

       List Keyframe





   Every iteration we have a

        List Spline

        We then want to merge these into the current `List Keyframe`

        We do this by putting each Spline in a separate Keyframe

        If they match times, everything works out (common case)

        If they do not match, split existing keyframes, mark sections as conflicting





-}


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


{-| -}
lerpCurvesCompoundHelper :
    List ( Id, Interpolate.State )
    -- previous end time
    -> Time.Absolute
    -- prev
    -> List Prop
    -- target
    -> List Prop
    -- target time
    -> Time.Absolute
    -- interrupted at
    -> Time.Absolute
    -> Maybe (Timeline.LookAhead (List Prop))
    -> Bool
    -> List Keyframe
    -> List ( Id, Interpolate.State )
    ->
        { keyframes : List Keyframe
        , conflicting : Bool
        , states : List ( Id, Interpolate.State )
        }
lerpCurvesCompoundHelper remainingStates prevEndTime prev target targetTime interruptedAt maybeLookAhead conflicted keyframes updatedStates =
    case remainingStates of
        [] ->
            { states = List.reverse updatedStates
            , keyframes = keyframes
            , conflicting = False
            }

        ( id, state ) :: remain ->
            let
                prevState =
                    stateOrDefault id "" prev

                targetState =
                    stateOrDefault id "" target

                lookAheadState =
                    Maybe.map
                        (Timeline.mapLookAhead
                            (stateOrDefault id "")
                        )
                        maybeLookAhead

                newState =
                    Interpolate.moving.transition
                        prevEndTime
                        prevState
                        targetState
                        targetTime
                        interruptedAt
                        lookAheadState
                        state

                _ =
                    Debug.log "LERP"
                        { prevEndTime = prevEndTime
                        , targetTime = targetTime
                        , target = target
                        , prev = prev
                        , interruptedAt = interruptedAt
                        , splines = splines
                        }

                splines =
                    Interpolate.transitionSplines
                        prevEndTime
                        targetState
                        targetTime
                        lookAheadState
                        state

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
            lerpCurvesCompoundHelper
                remain
                prevEndTime
                prev
                target
                targetTime
                interruptedAt
                maybeLookAhead
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


visitCurvesCompound :
    List ( Id, Interpolate.State )
    -> (state -> List Prop)
    -- previous
    -> Timeline.Occurring state
    -- target
    -> Timeline.Occurring state
    -- targetTime
    -> Time.Absolute
    -> Maybe (Timeline.LookAhead (List Prop))
    -> ( CompoundSection, List ( Id, Interpolate.State ) )
visitCurvesCompound states lookup previous target targetTime maybeLookahead =
    let
        -- _ =
        --     Debug.log "VISIT" ( previous, target )
        new =
            visitCurvesCompoundHelper states
                lookup
                previous
                target
                targetTime
                maybeLookahead
                False
                []
                []

        endPrevious =
            Timeline.endTime previous
    in
    ( { start = endPrevious
      , period = once (Time.duration endPrevious targetTime)
      , conflicting = new.conflicting
      , frames = new.frames
      }
    , new.states
    )


{-| -}
visitCurvesCompoundHelper :
    List ( Id, Interpolate.State )
    -> (state -> List Prop)
    -- previous
    -> Timeline.Occurring state
    -- target
    -> Timeline.Occurring state
    -- targetTime
    -> Time.Absolute
    -> Maybe (Timeline.LookAhead (List Prop))
    -> Bool
    -> List Keyframe
    -> List ( Id, Interpolate.State )
    ->
        { frames : List Keyframe
        , conflicting : Bool
        , states : List ( Id, Interpolate.State )
        }
visitCurvesCompoundHelper remainingStates lookup previous target targetTime maybeLookAhead conflicting keyframes updatedStates =
    case remainingStates of
        [] ->
            { states = List.reverse updatedStates
            , frames = keyframes
            , conflicting = conflicting
            }

        ( id, state ) :: remain ->
            let
                newState =
                    Interpolate.moving.visit
                        (\s ->
                            -- compound curves only deal in transforms, which have ids
                            lookup s
                                |> stateOrDefault id ""
                        )
                        target
                        targetTime
                        (Maybe.map
                            (Timeline.mapLookAhead
                                (stateOrDefault id "")
                            )
                            maybeLookAhead
                        )
                        state

                previousMovement =
                    lookup (Timeline.getEvent previous)
                        |> stateOrDefault id ""

                targetMovement =
                    lookup (Timeline.getEvent target)
                        |> stateOrDefault id ""

                splines =
                    -- NOTE, we could check movement equality here
                    -- to resolve to stationary
                    Interpolate.transitionSplines
                        (Timeline.endTime previous)
                        targetMovement
                        targetTime
                        (Maybe.map
                            (Timeline.mapLookAhead
                                (stateOrDefault id "")
                            )
                            maybeLookAhead
                        )
                        state

                new =
                    case keyframes of
                        [] ->
                            { keyframes =
                                List.map
                                    (splineToKeyframe id)
                                    splines
                            , conflicting = conflicting
                            }

                        _ ->
                            mergeIntoKeyframes conflicting
                                id
                                splines
                                []
                                keyframes
            in
            visitCurvesCompoundHelper
                remain
                lookup
                previous
                target
                targetTime
                maybeLookAhead
                new.conflicting
                new.keyframes
                (( id, newState ) :: updatedStates)


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
                    Debug.log "SPLINES" <|
                        Interpolate.transitionSplines
                            (Timeline.endTime prev)
                            (lookup (Timeline.getEvent target))
                            targetTime
                            maybeLookAhead
                            state

                sections =
                    case existingSections of
                        [] ->
                            [ Section
                                { start = Timeline.endTime prev
                                , period = once (Time.duration (Timeline.endTime prev) targetTime)
                                , splines = visitSplines
                                }
                            ]

                        (Section top) :: remain ->
                            case top.period of
                                Timeline.Repeat 1 dur ->
                                    Section
                                        { start = top.start
                                        , period = once (Time.expand dur (Time.duration (Timeline.endTime prev) targetTime))
                                        , splines = top.splines ++ visitSplines
                                        }
                                        :: remain

                                _ ->
                                    existingSections
                                        |> (::)
                                            (Section
                                                { start = Timeline.endTime prev
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


once : Time.Duration -> Timeline.Period
once =
    Timeline.Repeat 1


toCurvesLerp :
    Time.Absolute
    -> Interpolate.Movement
    -> Time.Absolute
    -> Time.Absolute
    -> Maybe (Timeline.LookAhead Interpolate.Movement)
    -> Interpolate.State
    -> List Section
    -> List Section
toCurvesLerp prevEndTime target targetTime interruptedAt maybeLookAhead state existingSections =
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
            -- let
            --     _ =
            --         Debug.log "    : "
            --             { prevEndTime = prevEndTime
            --             , prev = prev
            --             , target = target
            --             , targetTime = targetTime
            --             , la = maybeLookAhead
            --             , state = state
            --             }
            -- in
            -- Debug.log "LERP SPLINES" <|
            Interpolate.transitionSplines
                prevEndTime
                target
                targetTime
                maybeLookAhead
                state

        dur =
            Time.duration prevEndTime targetTime

        sliced =
            if interruptedAt == targetTime then
                transitionSplines

            else
                Bezier.takeBefore (Time.inMilliseconds interruptedAt) transitionSplines
    in
    existingSections
        |> (::)
            (Section
                { start = prevEndTime
                , period = once dur
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
        Interpolate.Pos _ _ Nothing ->
            existing

        Interpolate.Pos _ val (Just seq) ->
            Section
                { start = startTime
                , period =
                    Interpolate.sequenceToPeriod seq
                , splines =
                    Interpolate.sequenceToSplines startTime val seq
                }
                :: existing


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


{-| -}
type alias CssAnim =
    { hash : String

    -- use single prop encoding:
    -- https://developer.mozilla.org/en-US/docs/Web/CSS/animation
    , animation : String
    , keyframes : String
    }


combine : CssAnim -> CssAnim -> CssAnim
combine one two =
    if String.isEmpty one.hash then
        two

    else if String.isEmpty two.hash then
        one

    else
        { hash = one.hash ++ two.hash
        , animation = two.animation ++ ", " ++ one.animation
        , keyframes = one.keyframes ++ "\n" ++ two.keyframes
        }
