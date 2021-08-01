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


cssFromProps : Timeline.Timeline state -> (state -> List Prop) -> CssAnim
cssFromProps timeline lookup =
    let
        present =
            getInitial timeline lookup

        renderedProps =
            Timeline.foldpAll2 lookup
                (\_ -> present)
                toPropCurves2
                timeline
    in
    renderCss (Timeline.getCurrentTime timeline) renderers renderedProps


getInitial : Timeline.Timeline event -> (event -> List Prop) -> List RenderedProp
getInitial timeline lookup =
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

        RenderedColorProp details ->
            False

        TransformProp details ->
            False


matchColor name renderedProp =
    case renderedProp of
        RenderedProp details ->
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


{-| RenderdProp's are required to be ordered!
-}
type alias Renderer =
    Time.Absolute -> List RenderedProp -> Maybe ( CssAnim, List RenderedProp )


renderers : List Renderer
renderers =
    [ scalars
    ]


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
        ++ String.fromInt (round (trans.rotation * 100))
        ++ "-"
        ++ String.fromInt (round (trans.scale * 100))


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
        )
        cursor


getCommonTransformTransition : List Prop -> Transition.Transition
getCommonTransformTransition props =
    case props of
        [] ->
            Transition.standard

        (Prop _ _ (Move.Pos trans _ _) _) :: remain ->
            if Transition.isStandard trans then
                getCommonTransformTransition remain

            else
                trans

        (ColorProp _ (Move.Pos trans _ _) _) :: remain ->
            getCommonTransformTransition remain


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
