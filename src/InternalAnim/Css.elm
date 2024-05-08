module InternalAnim.Css exposing
    ( Prop(..)
    , RenderedProp(..)
    , match
    , propsToRenderedProps
    , renderedPropHasSequence
    , toCss
    )

{-| -}

import Color
import InternalAnim.Css.Props as Props
import InternalAnim.Duration as Duration
import InternalAnim.Hash as Hash
import InternalAnim.Move as Move
import InternalAnim.Time as Time
import InternalAnim.Timeline as Timeline
import InternalAnim.Transition as Transition
import InternalAnim.Units as Units


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


isGroupProp : Id -> Prop -> Bool
isGroupProp groupId prop =
    case prop of
        Prop id _ _ _ ->
            Props.isGroup groupId id

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
    Timeline.foldpAll (Timeline.getUpdatedAt timeline)
        lookup
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


toCss :
    { now : Time.Absolute
    , attrs : List RenderedProp
    , allowTransitions : Move.AllowTransitions
    }
    -> Css
toCss { allowTransitions, now, attrs } =
    let
        cssDetails =
            props2Css allowTransitions now attrs emptyAnim
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


getInitial : Timeline.Timeline event -> (event -> List Prop) -> List RenderedProp
getInitial timeline lookup =
    let
        rendered =
            Timeline.foldpAll (Timeline.getUpdatedAt timeline)
                lookup
                (\props ->
                    toInitialProps props { props = [], translation = Nothing, scale = Nothing }
                )
                (\get _ target _ _ _ _ cursor ->
                    addInitialProps (get (Timeline.getEvent target)) cursor
                )
                timeline
    in
    rendered.props
        |> addMaybeVector rendered.translation
        |> addMaybeVector rendered.scale


addMaybeVector : Maybe VectorDetails -> List RenderedProp -> List RenderedProp
addMaybeVector maybeVector renderedProps =
    case maybeVector of
        Nothing ->
            renderedProps

        Just vector ->
            VectorProp vector :: renderedProps


initState : Float -> Move.State
initState x =
    { position =
        Units.pixels x
    , velocity = Units.pixelsPerSecond 0
    }


type alias AllRenderedProps =
    { props : List RenderedProp
    , translation : Maybe VectorDetails
    , scale : Maybe VectorDetails
    }


initVector : Float -> VectorState
initVector i =
    { x = initState i
    , y = initState i
    , z = initState i
    }


toInitialProps : List Prop -> AllRenderedProps -> AllRenderedProps
toInitialProps props rendered =
    case props of
        [] ->
            rendered

        (Prop id name movement format) :: remaining ->
            toInitialProps remaining
                (if Props.isTranslateId id then
                    case rendered.translation of
                        Nothing ->
                            { props = rendered.props
                            , translation =
                                Just
                                    { group = Props.groups.translation
                                    , name = name
                                    , format = format
                                    , sections = []
                                    , state = Props.initVectorState id (Move.init movement)
                                    }
                            , scale = rendered.scale
                            }

                        Just translation ->
                            -- we've already initialized the transform
                            -- add the new prop to the list
                            { props = rendered.props
                            , translation =
                                Just
                                    { translation
                                        | state = Props.updateVectorById id (Move.init movement) translation.state
                                    }
                            , scale = rendered.scale
                            }

                 else if Props.isScaleId id then
                    case rendered.scale of
                        Nothing ->
                            { props = rendered.props
                            , translation = rendered.scale
                            , scale =
                                Just
                                    { group = Props.groups.scaling
                                    , name = name
                                    , format = format
                                    , sections = []
                                    , state = Props.initVectorState id (Move.init movement)
                                    }
                            }

                        Just scale ->
                            -- we've already initialized the transform
                            { props = rendered.props
                            , scale =
                                Just
                                    { scale
                                        | state = Props.updateVectorById id (Move.init movement) scale.state
                                    }
                            , translation = rendered.translation
                            }

                 else
                    let
                        state =
                            Move.init
                                (Props.default id)
                    in
                    { props =
                        RenderedProp
                            { id = id
                            , name = name
                            , format = format
                            , sections = []
                            , state = state
                            }
                            :: rendered.props
                    , translation = rendered.translation
                    , scale = rendered.scale
                    }
                )

        (ColorProp name (Move.Pos _ color _)) :: remaining ->
            toInitialProps remaining
                { props =
                    RenderedColorProp
                        { name = name
                        , color = color
                        , sections = []
                        }
                        :: rendered.props
                , translation = rendered.translation
                , scale = rendered.scale
                }


matchProp : Id -> RenderedProp -> Bool
matchProp id renderedProp =
    case renderedProp of
        RenderedProp details ->
            details.id - id == 0

        RenderedColorProp _ ->
            False

        VectorProp _ ->
            False


matchColor : String -> RenderedProp -> Bool
matchColor name renderedProp =
    case renderedProp of
        RenderedProp _ ->
            False

        RenderedColorProp details ->
            details.name == name

        VectorProp _ ->
            False


{-| If a props isn't defined in the first state, but is defined in the future, we want to add it.
-}
addInitialProps : List Prop -> AllRenderedProps -> AllRenderedProps
addInitialProps props rendered =
    case props of
        [] ->
            rendered

        (Prop id name _ format) :: remaining ->
            let
                new =
                    if Props.isTranslateId id then
                        case rendered.translation of
                            Nothing ->
                                { props = rendered.props
                                , translation =
                                    Just
                                        { group = Props.groups.translation
                                        , name = name
                                        , format = format
                                        , sections = []
                                        , state = initVector 0
                                        }
                                , scale = rendered.scale
                                }

                            Just _ ->
                                -- we've already initialized the transform
                                rendered

                    else if Props.isScaleId id then
                        case rendered.scale of
                            Nothing ->
                                { props = rendered.props
                                , translation = rendered.scale
                                , scale =
                                    Just
                                        { group = Props.groups.scaling
                                        , name = name
                                        , format = format
                                        , sections = []
                                        , state = initVector 1
                                        }
                                }

                            Just _ ->
                                -- we've already initialized the transform
                                rendered

                    else if List.any (\renderedProp -> matchProp id renderedProp) rendered.props then
                        rendered

                    else
                        { props =
                            RenderedProp
                                { id = id
                                , name = name
                                , format = format
                                , sections = []
                                , state = Move.init (Props.default id)
                                }
                                :: rendered.props
                        , translation = rendered.translation
                        , scale = rendered.scale
                        }
            in
            addInitialProps remaining
                new

        (ColorProp name (Move.Pos _ color _)) :: remaining ->
            let
                new =
                    if List.any (\renderedProp -> matchColor name renderedProp) rendered.props then
                        rendered

                    else
                        { props =
                            RenderedColorProp
                                { name = name
                                , color = color
                                , sections = []
                                }
                                :: rendered.props
                        , translation = rendered.translation
                        , scale = rendered.scale
                        }
            in
            addInitialProps remaining
                new


props2Css : Move.AllowTransitions -> Time.Absolute -> List RenderedProp -> CssAnim -> CssAnim
props2Css allowTransitions now renderedProps anim =
    case renderedProps of
        [] ->
            anim

        (RenderedProp details) :: remain ->
            props2Css allowTransitions
                now
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
                            allowTransitions
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
            props2Css allowTransitions
                now
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
                            allowTransitions
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

        (VectorProp details) :: remain ->
            props2Css allowTransitions
                now
                remain
                (case details.sections of
                    [] ->
                        { anim
                            | hash = vectorToHash details.group (vectorStateToVector details.state) ++ anim.hash
                            , props =
                                ( details.name
                                , Props.vectorToString details.group (vectorStateToVector details.state)
                                )
                                    :: anim.props
                        }

                    _ ->
                        Move.cssForSections now
                            allowTransitions
                            (vectorStateToVector details.state)
                            details.name
                            (\t one two ->
                                details.name
                                    ++ ": "
                                    ++ Props.vectorToString details.group
                                        (Move.lerpVector t one two)
                            )
                            (Props.vectorToString details.group)
                            (vectorToHash details.group)
                            (List.reverse details.sections)
                            emptyAnim
                            |> combine anim
                )


vectorToHash : Props.Id -> Vector -> String
vectorToHash group vec =
    if vec.x == 0 && vec.y == 0 && vec.z == 0 then
        ""

    else
        let
            tag =
                case group of
                    10 ->
                        "t"

                    20 ->
                        "s"

                    _ ->
                        "v"
        in
        tag
            ++ Hash.float vec.x
            ++ "-"
            ++ Hash.float vec.y
            ++ "-"
            ++ Hash.float vec.z


vectorStateToVector : VectorState -> Vector
vectorStateToVector state =
    { x =
        Units.inPixels state.x.position
    , y =
        Units.inPixels state.y.position
    , z =
        Units.inPixels state.z.position
    }


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
                    in
                    RenderedProp
                        { id = rendered.id
                        , name = rendered.name
                        , format = rendered.format
                        , sections =
                            if finished then
                                rendered.sections

                            else
                                let
                                    finalProp =
                                        -- this is the check for being a transition
                                        if not (Time.equal (Timeline.endTime prev) startTime) then
                                            -- adjust the transition by taking into account
                                            -- the intro and exit velocity
                                            -- but only if this is an interruption
                                            let
                                                startPosition =
                                                    Units.inPixels rendered.state.position

                                                targetPosition =
                                                    case targetProp of
                                                        Move.Pos _ x _ ->
                                                            x
                                            in
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
                                in
                                Move.sequences
                                    startTime
                                    targetTime
                                    now
                                    endTime
                                    finalProp
                                    rendered.sections
                        , state =
                            rendered.state
                                |> Move.at progress
                                    startTime
                                    targetTime
                                    targetProp
                        }

                VectorProp details ->
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
                                |> List.filter (isGroupProp details.group)

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
                                                (Units.inPixels details.state.z.position)
                                                targets.z
                                                details.state.z.velocity
                                            ]
                                in
                                getCommonTransformTransition
                                    targetProps
                                    Transition.standard

                            else
                                getCommonTransformTransition
                                    targetProps
                                    Transition.standard

                        targets =
                            { x =
                                getVectorSlot details.group Props.X targetProps
                            , y =
                                getVectorSlot details.group Props.Y targetProps
                            , z =
                                getVectorSlot details.group Props.Z targetProps
                            }
                    in
                    VectorProp
                        { group = details.group
                        , name = details.name
                        , format = details.format
                        , sections =
                            if finished then
                                details.sections

                            else
                                let
                                    commonSequence =
                                        getCommonVectorSequence details.group
                                            targetProps
                                            []

                                    commonMovement =
                                        Move.move commonTransition
                                            targets
                                            commonSequence
                                in
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
                            , z =
                                Move.at progress
                                    startTime
                                    targetTime
                                    (Move.toWith commonTransition
                                        targets.z
                                    )
                                    details.state.z
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
getCommonVectorSequence :
    Props.Id
    -> List Prop
    -> List (Move.Sequence Vector)
    -> List (Move.Sequence Vector)
getCommonVectorSequence groupId props sequences =
    case props of
        (Prop _ _ (Move.Pos _ _ propSeq) _) :: _ ->
            -- sequences
            vectorSeq groupId props propSeq 0 []

        _ ->
            sequences


vectorSeq :
    Id
    -> List Prop
    -> List (Move.Sequence Float)
    -> Int
    -> List (Move.Sequence Vector)
    -> List (Move.Sequence Vector)
vectorSeq groupId props pilotSequence seqLevel renderedSeq =
    case pilotSequence of
        [] ->
            renderedSeq

        (Move.Sequence n delay dur steps) :: remain ->
            vectorSeq groupId
                props
                remain
                (seqLevel + 1)
                (Move.Sequence n
                    delay
                    dur
                    (gatherVectorSteps groupId
                        seqLevel
                        0
                        steps
                        props
                        []
                    )
                    :: renderedSeq
                )


gatherVectorSteps :
    Id
    -> Int
    -> Int
    -> List (Move.Step Float)
    -> List Prop
    -> List (Move.Step Vector)
    -> List (Move.Step Vector)
gatherVectorSteps groupId seqLevel stepLevel steps props transforms =
    case steps of
        [] ->
            transforms

        (Move.Step dur trans _) :: remainingSteps ->
            gatherVectorSteps groupId
                seqLevel
                (stepLevel + 1)
                remainingSteps
                props
                (getVectorStepAt groupId dur trans seqLevel stepLevel props
                    :: transforms
                )


getVectorStepAt : Id -> Duration.Duration -> Transition.Transition -> Int -> Int -> List Prop -> Move.Step Vector
getVectorStepAt groupId dur trans seqLevel stepLevel props =
    let
        x =
            Props.vectorSlotToId groupId Props.X

        y =
            Props.vectorSlotToId groupId Props.Y

        z =
            Props.vectorSlotToId groupId Props.Z
    in
    Move.Step dur
        trans
        { x =
            getTransformSequenceValueAt seqLevel
                stepLevel
                (Props.groupToCompoundId groupId)
                x
                props
                Nothing
        , y =
            getTransformSequenceValueAt seqLevel
                stepLevel
                (Props.groupToCompoundId groupId)
                y
                props
                Nothing
        , z =
            getTransformSequenceValueAt seqLevel
                stepLevel
                (Props.groupToCompoundId groupId)
                z
                props
                Nothing
        }



{- END VECTOR -}


getTransformSequenceValueAt : Int -> Int -> Maybe Props.Id -> Props.Id -> List Prop -> Maybe Float -> Float
getTransformSequenceValueAt seqLevel stepLevel maybeDefaultId targetId props defaultValue =
    case props of
        [] ->
            case defaultValue of
                Nothing ->
                    Props.defaultPosition targetId

                Just default ->
                    default

        (Prop id _ move _) :: remain ->
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
                case maybeDefaultId of
                    Nothing ->
                        getTransformSequenceValueAt seqLevel stepLevel maybeDefaultId targetId remain defaultValue

                    Just defaultId ->
                        if id - defaultId == 0 then
                            let
                                newDefaultValue =
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
                            in
                            getTransformSequenceValueAt seqLevel stepLevel Nothing targetId remain (Just newDefaultValue)

                        else
                            getTransformSequenceValueAt seqLevel stepLevel maybeDefaultId targetId remain defaultValue

        (ColorProp _ _) :: remain ->
            getTransformSequenceValueAt seqLevel stepLevel maybeDefaultId targetId remain defaultValue


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


getCommonTransformTransition :
    List Prop
    -> Transition.Transition
    -> Transition.Transition
getCommonTransformTransition props currentTrans =
    case props of
        [] ->
            currentTrans

        (Prop _ _ (Move.Pos trans _ _) _) :: remain ->
            if Transition.isStandard trans then
                getCommonTransformTransition remain currentTrans

            else
                getCommonTransformTransition remain trans

        (ColorProp _ (Move.Pos trans _ _)) :: remain ->
            getCommonTransformTransition remain trans


getVectorSlot : Id -> Props.VectorSlot -> List Prop -> Float
getVectorSlot groupId slot props =
    valueOrDefault (Props.groupToCompoundId groupId)
        (Props.vectorSlotToId groupId slot)
        props
        Nothing


{-| -}
valueOrDefault : Maybe Id -> Id -> List Prop -> Maybe Float -> Float
valueOrDefault maybeDefaultid targetId props defaultVal =
    case props of
        [] ->
            case defaultVal of
                Nothing ->
                    Props.defaultPosition targetId

                Just val ->
                    val

        (Prop id _ move _) :: remain ->
            if id - targetId == 0 then
                case move of
                    Move.Pos _ v _ ->
                        v

            else
                case maybeDefaultid of
                    Nothing ->
                        valueOrDefault maybeDefaultid targetId remain defaultVal

                    Just defaultId ->
                        if id - defaultId == 0 then
                            case move of
                                Move.Pos _ v _ ->
                                    valueOrDefault Nothing targetId remain (Just v)

                        else
                            valueOrDefault maybeDefaultid targetId remain defaultVal

        (ColorProp _ _) :: remain ->
            valueOrDefault maybeDefaultid targetId remain defaultVal


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

        (ColorProp _ _) :: remain ->
            stateOrDefault targetId targetName remain


{-| -}
colorOrDefault : String -> Color.Color -> List Prop -> Color.Color
colorOrDefault targetName default props =
    case props of
        [] ->
            default

        (Prop _ _ _ _) :: remain ->
            colorOrDefault targetName default remain

        (ColorProp name (Move.Pos _ clr _)) :: remain ->
            if targetName == name then
                clr

            else
                colorOrDefault targetName default remain


{-| A group of curves represents the trail of one scalar property

    (Scalar property meaning something like opacity, or just the `R` channel of rgb.)

-}
type RenderedProp
    = RenderedProp RenderedPropDetails
    | RenderedColorProp RenderedColorPropDetails
      -- transform can now be deconstructed into its parts
      -- This is for translation and scaling
      -- Rotation is a RenderedProp
    | VectorProp VectorDetails


renderedPropHasSequence : RenderedProp -> Bool
renderedPropHasSequence renderedProp =
    case renderedProp of
        RenderedProp details ->
            case details.sections of
                [] ->
                    False

                _ ->
                    True

        RenderedColorProp details ->
            case details.sections of
                [] ->
                    False

                _ ->
                    True

        VectorProp details ->
            case details.sections of
                [] ->
                    False

                _ ->
                    True


type alias Vector =
    { x : Float
    , y : Float
    , z : Float
    }


type alias VectorState =
    { x : Move.State
    , y : Move.State
    , z : Move.State
    }


type alias VectorDetails =
    { group : Id
    , name : String
    , format : Props.Format
    , sections : List (Move.Sequence Vector)
    , state : VectorState
    }


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
