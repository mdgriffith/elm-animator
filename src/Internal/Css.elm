module Internal.Css exposing
    ( Prop(..)
    , RenderedProp(..)
    , cssFromProps
    , match
    , propsToRenderedProps
    , toCss
    )

{-| -}

import Color
import Duration
import Html.Attributes exposing (id)
import Internal.Bezier as Bezier
import Internal.Bits as Bits
import Internal.Css.Props as Props
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
      Prop Id String (Move.Move Float) Props.Format
    | ColorProp String (Move.Move Color.Color)


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


toCss : Time.Absolute -> List RenderedProp -> CssAnim
toCss now renderedProps =
    props2Css now renderedProps emptyAnim


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
                            value =
                                Pixels.inPixels details.state.position
                                    |> Props.format details.format
                        in
                        { anim
                            | props =
                                ( details.name
                                , value
                                )
                                    :: anim.props
                        }

                    _ ->
                        Move.cssForSections now
                            (Pixels.inPixels details.state.position)
                            details.name
                            (Props.format details.format)
                            Move.floatToString
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
                            | props =
                                ( details.name
                                , Color.toCssString details.color
                                )
                                    :: anim.props
                        }

                    _ ->
                        Move.cssForSections now
                            details.color
                            details.name
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
                            | props =
                                ( "transform"
                                , renderTransformState details.state
                                )
                                    :: anim.props
                        }

                    _ ->
                        Move.cssForSections now
                            (stateToTransform details.state)
                            "transform"
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


renderTransformState :
    { a
        | x : { b | position : Quantity.Quantity Float Pixels.Pixels }
        , y : { c | position : Quantity.Quantity Float Pixels.Pixels }
        , rotation : { d | position : Quantity.Quantity Float Pixels.Pixels }
        , scale : { e | position : Quantity.Quantity Float Pixels.Pixels }
    }
    -> String
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
            Pixels.inPixelsPerSecond velocity
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
                            Move.color progress
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

                        commonTransition =
                            getCommonTransformTransition
                                targetProps
                                Transition.standard

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
                            if not (Time.equal (Timeline.endTime prev) startTime) then
                                -- this is a transition, we want to adjust the curve so it matches the transition
                                let
                                    fastestVelocity =
                                        firstNonZero
                                            [ normalizeVelocity
                                                startTime
                                                targetTime
                                                (Pixels.inPixels details.state.x.position)
                                                targets.x
                                                details.state.x.velocity
                                            , normalizeVelocity
                                                startTime
                                                targetTime
                                                (Pixels.inPixels details.state.y.position)
                                                targets.y
                                                details.state.y.velocity
                                            , normalizeVelocity
                                                startTime
                                                targetTime
                                                (Pixels.inPixels details.state.rotation.position)
                                                targets.rotation
                                                details.state.rotation.velocity
                                            , normalizeVelocity
                                                startTime
                                                targetTime
                                                (Pixels.inPixels details.state.scale.position)
                                                targets.scale
                                                details.state.scale.velocity
                                            ]
                                in
                                Move.toWith commonTransition targets
                                    |> Move.withVelocities fastestVelocity
                                        -- If we do any transition smoothing
                                        -- we'll need to normalize this velocity too
                                        --Estimation.velocityAtTarget lookupState target future
                                        0

                            else
                                Move.toWith commonTransition targets
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
