module InternalAnim.Css exposing
    ( Prop(..)
    , RenderedProp(..)
    , cssFromProps
    , match
    , propsToRenderedProps
    , toCss
    )

{-| -}

import Color
import Html
import Html.Attributes as Attr exposing (id)
import InternalAnim.Bezier as Bezier
import InternalAnim.Bits as Bits
import InternalAnim.Css.Props as Props
import InternalAnim.Duration as Duration
import InternalAnim.Move as Move
import InternalAnim.Quantity as Quantity
import InternalAnim.Time as Time
import InternalAnim.Timeline as Timeline
import InternalAnim.Transition as Transition
import InternalAnim.Units as Units
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
      Prop Id String (Move.Move Float) Props.Format
    | ColorProp String (Move.Move Color.Color)


isTransformProp prop =
    case prop of
        Prop id name _ _ ->
            Props.isTransformId id

        ColorProp _ _ ->
            False


match : Prop -> Prop -> Bool
match one two =
    case one of
        Prop id name _ _ ->
            case two of
                Prop twoId twoName _ _ ->
                    if Props.noId - id == 0 && Props.noId - twoId == 0 then
                        name == twoName

                    else
                        id - twoId == 0

                _ ->
                    False

        ColorProp name _ ->
            case two of
                ColorProp twoName _ ->
                    name == twoName

                _ ->
                    False


propsToRenderedProps : Timeline.Timeline state -> (state -> List Prop) -> List RenderedProp
propsToRenderedProps timeline lookup =
    let
        present =
            getInitial timeline lookup
    in
    Timeline.foldpAll lookup
        (\_ -> present)
        toPropCurves2
        timeline


{-| -}
type alias Css =
    { hash : String
    , keyframes : String
    , transition : String
    , props : List ( String, String )
    }


toCss : Time.Absolute -> List RenderedProp -> Css
toCss now renderedProps =
    let
        cssDetails =
            props2Css now renderedProps emptyAnim
    in
    { hash = cssDetails.hash
    , keyframes = cssDetails.keyframes
    , transition = cssDetails.transition
    , props =
        case cssDetails.animation of
            "" ->
                case cssDetails.transition of
                    "" ->
                        cssDetails.props

                    trans ->
                        ( "transition", trans ) :: cssDetails.props

            anim ->
                case cssDetails.transition of
                    "" ->
                        ( "animation", anim ) :: cssDetails.props

                    trans ->
                        ( "animation", anim ) :: ( "transition", trans ) :: cssDetails.props
    }


cssFromProps : Timeline.Timeline state -> (state -> List Prop) -> CssAnim
cssFromProps timeline lookup =
    let
        present =
            getInitial timeline lookup

        renderedProps =
            Timeline.foldpAll lookup
                (\_ -> present)
                toPropCurves2
                timeline
    in
    props2Css (Timeline.getCurrentTime timeline) renderedProps emptyAnim


getInitial : Timeline.Timeline event -> (event -> List Prop) -> List RenderedProp
getInitial timeline lookup =
    let
        ( maybeTransform, renderedProps ) =
            Timeline.foldpAll lookup
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
        Units.pixels x
    , velocity = Units.pixelsPerSecond 0
    }


toInitialProps : List Prop -> ( Maybe TransformPropDetails, List RenderedProp ) -> ( Maybe TransformPropDetails, List RenderedProp )
toInitialProps props (( maybeTransform, rendered ) as untouched) =
    case props of
        [] ->
            untouched

        (Prop id name movement format) :: remaining ->
            let
                state =
                    Move.init
                        (Props.default id)
            in
            toInitialProps remaining
                (if Props.isTransformId id then
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

        (ColorProp name (Move.Pos _ color _)) :: remaining ->
            toInitialProps remaining
                ( maybeTransform
                , RenderedColorProp
                    { name = name
                    , color = color
                    , sections = []
                    }
                    :: rendered
                )


matchProp : Id -> RenderedProp -> Bool
matchProp id renderedProp =
    case renderedProp of
        RenderedProp details ->
            details.id - id == 0

        RenderedColorProp details ->
            False

        TransformProp details ->
            False


matchColor : String -> RenderedProp -> Bool
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
                    if Props.isTransformId id then
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
                                Move.init
                                    (Props.default id)
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
            addInitialProps remaining
                new

        (ColorProp name (Move.Pos _ color _)) :: remaining ->
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
            addInitialProps remaining
                new


props2Css : Time.Absolute -> List RenderedProp -> CssAnim -> CssAnim
props2Css now renderedProps anim =
    case renderedProps of
        [] ->
            anim

        (RenderedProp details) :: remain ->
            props2Css now
                remain
                (case details.sections of
                    [] ->
                        let
                            val =
                                Units.inPixels details.state.position

                            value =
                                val
                                    |> Props.format details.format
                        in
                        { anim
                            | hash = Props.hash details val ++ anim.hash
                            , props =
                                ( details.name
                                , value
                                )
                                    :: anim.props
                        }

                    _ ->
                        Move.cssForSections now
                            (Units.inPixels details.state.position)
                            details.name
                            (\t one two ->
                                details.name
                                    ++ ": "
                                    ++ Props.format details.format
                                        (Move.lerpFloat t one two)
                            )
                            (Props.format details.format)
                            (Props.hash details)
                            (List.reverse details.sections)
                            emptyAnim
                            |> combine anim
                )

        (RenderedColorProp details) :: remain ->
            props2Css now
                remain
                (case details.sections of
                    [] ->
                        { anim
                            | hash = details.name ++ "-" ++ Props.colorHash details.color ++ anim.hash
                            , props =
                                ( details.name
                                , Color.toCssString details.color
                                )
                                    :: anim.props
                        }

                    _ ->
                        Move.cssForSections now
                            details.color
                            details.name
                            (\t one two ->
                                details.name
                                    ++ ": "
                                    ++ Color.toCssString
                                        (Move.lerpColor t one two)
                            )
                            Color.toCssString
                            Props.colorHash
                            (List.reverse details.sections)
                            emptyAnim
                            |> combine anim
                )

        (TransformProp details) :: remain ->
            props2Css now
                remain
                (case details.sections of
                    [] ->
                        { anim
                            | hash = transformToHash (stateToTransform details.state) ++ anim.hash
                            , props =
                                ( "transform"
                                , renderTransformState details.state
                                )
                                    :: anim.props
                        }

                    _ ->
                        Move.cssForSections now
                            (stateToTransform details.state)
                            "transform"
                            (\t one two ->
                                "transform: "
                                    ++ transformToString
                                        (Move.lerpTransform t one two)
                            )
                            transformToString
                            transformToHash
                            (List.reverse details.sections)
                            emptyAnim
                            |> combine anim
                )


transformToHash : { a | x : Float, y : Float, rotation : Float, scale : Float } -> String
transformToHash trans =
    "t-"
        ++ String.fromInt (round trans.x)
        ++ "-"
        ++ String.fromInt (round trans.y)
        ++ "-"
        ++ String.fromInt (round (trans.rotation * 100))
        ++ "-"
        ++ String.fromInt (round (trans.scale * 100))


stateToTransform : TransformState -> Transform
stateToTransform state =
    { x =
        Units.inPixels state.x.position
    , y =
        Units.inPixels state.y.position
    , scale =
        Units.inPixels state.scale.position
    , rotation =
        Units.inPixels state.rotation.position
    }


renderTransformState :
    { a
        | x : { b | position : Units.Pixels }
        , y : { c | position : Units.Pixels }
        , rotation : { d | position : Units.Pixels }
        , scale : { e | position : Units.Pixels }
    }
    -> String
renderTransformState state =
    "translate("
        ++ String.fromFloat (Units.inPixels state.x.position)
        ++ "px, "
        ++ String.fromFloat (Units.inPixels state.y.position)
        ++ "px) rotate("
        ++ String.fromFloat (Units.inPixels state.rotation.position)
        ++ "turn)"
        ++ " scale("
        ++ String.fromFloat (Units.inPixels state.scale.position)
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
    , transition = ""
    , keyframes = ""
    , props = []
    }


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
            Units.inPixelsPerSecond velocity
    in
    if pixelsPerSecond == 0 then
        0

    else
        (pixelsPerSecond * Duration.inSeconds (Time.duration startTime targetTime))
            / (targetPosition - startPosition)


{-| -}
toPropCurves2 : Timeline.Transition state (List Prop) (List RenderedProp)
toPropCurves2 lookup prev target now startTime endTime future cursor =
    let
        targetTime =
            Timeline.startTime target

        progress =
            Time.progress startTime targetTime now

        finished =
            -- we only want to ignore this event if it's both finished
            -- and not immediately preceding an event that is still upcoming
            --case future of
            --    [] ->
            --        False
            --
            --    next :: _ ->
            -- Time.thisAfterOrEqualThat now (Timeline.endTime target)
            --&& not (Time.thisBeforeThat now (Timeline.startTime next))
            ---------
            --  Time.thisAfterOrEqualThat now (Timeline.endTime target)
            Time.thisAfterThat now (Timeline.endTime target)

        -- || Time.equal now ()
    in
    List.map
        (\prop ->
            case prop of
                RenderedColorProp details ->
                    let
                        targetColor =
                            colorOrDefault details.name
                                Props.transparent
                                (lookup (Timeline.getEvent target))
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
                                        targetTime
                                        now
                                        endTime
                                        (Move.toWith Transition.linear targetColor)
                        , color =
                            Move.lerpColor progress
                                details.color
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

                        finalProp =
                            -- this is the check for being a transition
                            if not (Time.equal (Timeline.endTime prev) startTime) then
                                -- adjust the transition by taking into account
                                -- the intro and exit velocity
                                -- but only if this is an interruption
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
                                        --Estimation.velocityAtTarget lookupState target future
                                        0

                            else
                                targetProp

                        startPosition =
                            Units.inPixels rendered.state.position

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
                            rendered.state
                                |> Move.transitionTo progress
                                    startTime
                                    targetTime
                                    targetProp
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
                                |> List.filter isTransformProp

                        commonTransition =
                            if not (Time.equal (Timeline.endTime prev) startTime) then
                                let
                                    fastestVelocity =
                                        firstNonZero
                                            [ normalizeVelocity
                                                startTime
                                                targetTime
                                                (Units.inPixels details.state.x.position)
                                                targets.x
                                                details.state.x.velocity
                                            , normalizeVelocity
                                                startTime
                                                targetTime
                                                (Units.inPixels details.state.y.position)
                                                targets.y
                                                details.state.y.velocity
                                            , normalizeVelocity
                                                startTime
                                                targetTime
                                                (Units.inPixels details.state.rotation.position)
                                                targets.rotation
                                                details.state.rotation.velocity
                                            , normalizeVelocity
                                                startTime
                                                targetTime
                                                (Units.inPixels details.state.scale.position)
                                                targets.scale
                                                details.state.scale.velocity
                                            ]
                                in
                                getCommonTransformTransition
                                    targetProps
                                    Transition.standard
                                    |> Transition.withVelocities fastestVelocity
                                        -- If we do any transition smoothing
                                        -- we'll need to normalize this velocity too
                                        --Estimation.velocityAtTarget lookupState target future
                                        0

                            else
                                getCommonTransformTransition
                                    targetProps
                                    Transition.standard

                        commonSequence =
                            getCommonTransformSequence
                                targetProps
                                []

                        targets =
                            { x =
                                transformOrDefault Props.ids.x
                                    targetProps
                            , y =
                                transformOrDefault Props.ids.y
                                    targetProps
                            , scale =
                                transformOrDefault Props.ids.scale
                                    targetProps
                            , rotation =
                                transformOrDefault Props.ids.rotation
                                    targetProps
                            }

                        commonMovement =
                            Move.move commonTransition
                                targets
                                commonSequence
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
                                    commonMovement
                                    details.sections
                        , state =
                            { x =
                                Move.at progress
                                    startTime
                                    targetTime
                                    (Move.toWith commonTransition
                                        targets.x
                                    )
                                    details.state.x
                            , y =
                                Move.at progress
                                    startTime
                                    targetTime
                                    (Move.toWith commonTransition
                                        targets.y
                                    )
                                    details.state.y
                            , scale =
                                Move.at progress
                                    startTime
                                    targetTime
                                    (Move.toWith commonTransition
                                        targets.scale
                                    )
                                    details.state.scale
                            , rotation =
                                Move.at progress
                                    startTime
                                    targetTime
                                    (Move.toWith commonTransition
                                        targets.rotation
                                    )
                                    details.state.rotation
                            }
                        }
        )
        cursor


firstNonZero : List Float -> Float
firstNonZero list =
    case list of
        [] ->
            0

        top :: remain ->
            if top /= 0 then
                top

            else
                firstNonZero remain


{-|

    *warning! this need to be called with pre-filtered props that are only transform props!

-}
getCommonTransformSequence : List Prop -> List (Move.Sequence Transform) -> List (Move.Sequence Transform)
getCommonTransformSequence props sequences =
    case props of
        (Prop id name (Move.Pos trans v propSeq) format) :: _ ->
            -- sequences
            transformSeq props propSeq 0 []

        _ ->
            sequences


transformSeq : List Prop -> List (Move.Sequence Float) -> Int -> List (Move.Sequence Transform) -> List (Move.Sequence Transform)
transformSeq props pilotSequence seqLevel renderedTransforms =
    case pilotSequence of
        [] ->
            renderedTransforms

        (Move.Sequence n delay dur steps) :: remain ->
            transformSeq props
                remain
                (seqLevel + 1)
                (Move.Sequence n
                    delay
                    dur
                    (gatherSequenceSteps seqLevel
                        0
                        steps
                        props
                        []
                    )
                    :: renderedTransforms
                )


gatherSequenceSteps :
    Int
    -> Int
    -> List (Move.Step Float)
    -> List Prop
    -> List (Move.Step Transform)
    -> List (Move.Step Transform)
gatherSequenceSteps seqLevel stepLevel steps props transforms =
    case steps of
        [] ->
            transforms

        (Move.Step dur trans target) :: remainingSteps ->
            gatherSequenceSteps seqLevel
                (stepLevel + 1)
                remainingSteps
                props
                (getTransformStepAt dur trans seqLevel stepLevel props
                    :: transforms
                )


getTransformStepAt dur trans seqLevel stepLevel props =
    Move.Step dur
        trans
        { x =
            getTransformSequenceValueAt seqLevel
                stepLevel
                Props.ids.x
                props
        , y =
            getTransformSequenceValueAt seqLevel
                stepLevel
                Props.ids.y
                props
        , scale =
            getTransformSequenceValueAt seqLevel
                stepLevel
                Props.ids.scale
                props
        , rotation =
            getTransformSequenceValueAt seqLevel
                stepLevel
                Props.ids.rotation
                props
        }


getTransformSequenceValueAt : Int -> Int -> Props.Id -> List Prop -> Float
getTransformSequenceValueAt seqLevel stepLevel targetId props =
    case props of
        [] ->
            Props.defaultPosition targetId

        (Prop id name move _) :: remain ->
            if id - targetId == 0 then
                case move of
                    Move.Pos _ v seq ->
                        case getAt seqLevel seq of
                            Nothing ->
                                v

                            Just (Move.Sequence _ _ _ seqSteps) ->
                                case getAt stepLevel seqSteps of
                                    Nothing ->
                                        v

                                    Just (Move.Step _ _ stepValue) ->
                                        stepValue

            else
                getTransformSequenceValueAt seqLevel stepLevel targetId remain

        (ColorProp name movement) :: remain ->
            getTransformSequenceValueAt seqLevel stepLevel targetId remain


getAt : Int -> List a -> Maybe a
getAt i list =
    case list of
        [] ->
            Nothing

        top :: remain ->
            if i == 0 then
                Just top

            else
                getAt (i - 1) remain


getCommonTransformTransition : List Prop -> Transition.Transition -> Transition.Transition
getCommonTransformTransition props currentTrans =
    case props of
        [] ->
            currentTrans

        (Prop id _ (Move.Pos trans _ _) _) :: remain ->
            if Transition.isStandard trans then
                getCommonTransformTransition remain currentTrans

            else
                getCommonTransformTransition remain trans

        (ColorProp _ (Move.Pos trans _ _)) :: remain ->
            getCommonTransformTransition remain trans


{-| -}
transformOrDefault : Id -> List Prop -> Float
transformOrDefault targetId props =
    case props of
        [] ->
            Props.defaultPosition targetId

        (Prop id name move _) :: remain ->
            if id - targetId == 0 then
                case move of
                    Move.Pos _ v _ ->
                        v

            else
                transformOrDefault targetId remain

        (ColorProp name movement) :: remain ->
            transformOrDefault targetId remain


{-| -}
stateOrDefault : Id -> String -> List Prop -> Move.Move Float
stateOrDefault targetId targetName props =
    case props of
        [] ->
            Props.default targetId

        (Prop id name move _) :: remain ->
            if (targetId - Props.noId) == 0 then
                if name == targetName then
                    move

                else
                    stateOrDefault targetId targetName remain

            else if id == targetId then
                move

            else
                stateOrDefault targetId targetName remain

        (ColorProp name movement) :: remain ->
            stateOrDefault targetId targetName remain


{-| -}
colorOrDefault : String -> Color.Color -> List Prop -> Color.Color
colorOrDefault targetName default props =
    case props of
        [] ->
            default

        (Prop id _ move _) :: remain ->
            colorOrDefault targetName default remain

        (ColorProp name (Move.Pos _ clr _)) :: remain ->
            if targetName == name then
                clr

            else
                colorOrDefault name default remain


matchForMovement : Id -> String -> List Prop -> Maybe (Move.Move Float)
matchForMovement onlyId onlyName props =
    case props of
        [] ->
            Nothing

        (ColorProp name move) :: remain ->
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


type alias RenderedPropDetails =
    { id : Id
    , name : String
    , format : Props.Format
    , sections :
        List (Move.Sequence Float)
    , state : Move.State
    }


type alias RenderedColorPropDetails =
    { name : String
    , color : Color.Color
    , sections :
        List (Move.Sequence Color.Color)
    }


type alias TransformPropDetails =
    { sections :
        List (Move.Sequence Transform)
    , state : TransformState
    }


type alias TransformState =
    { x : Move.State
    , y : Move.State
    , scale : Move.State
    , rotation : Move.State
    }


type alias Transform =
    { x : Float
    , y : Float
    , scale : Float
    , rotation : Float
    }


type alias TransformPresence =
    Bits.Bits Transform


{-| Slightly different than CssAnim in that we can also have style properties
This is for when a property has not changed and so a full animation is not necessary
-}
type alias CssAnim =
    { hash : String

    -- use single prop encoding:
    -- https://developer.mozilla.org/en-US/docs/Web/CSS/animation
    , animation : String

    -- same, this is all the transitions needed to render this anim
    , transition : String
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
