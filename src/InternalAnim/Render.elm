module InternalAnim.Render exposing (Css, Step(..), keyframes, onTimeline, transition)

{-| All CSS animation enters here. Properties are grouped once, then timeline
events and explicit steps are rendered as chronological keyframe animations.
Only the explicit `transition` entry point permits native CSS transitions.

Each animation has explicit endpoints and a delay relative to the last schedule
update. Later animations override earlier ones only when they start (forwards
fill). This also lets an interruption replace an infinite resting animation.

-}

import Bezier
import Bitwise
import Color
import Dict exposing (Dict)
import InternalAnim.Css.Props as Props
import InternalAnim.Duration as Duration
import InternalAnim.Move as Move
import InternalAnim.Property exposing (Prop(..))
import InternalAnim.Render.Css as Css
import InternalAnim.Time as Time
import InternalAnim.Timeline as Timeline
import InternalAnim.Transition as Transition
import InternalAnim.Units as Units


type alias Css =
    { hash : String
    , keyframes : String
    , transition : String
    , props : List ( String, String )
    }


type Step
    = Step Time.Duration (List Prop)
    | Repeat Int (List Step)


type Format
    = Scalar Props.Format
    | Translation
    | Scaling
    | Rgba


type alias Channel =
    { position : Float
    , velocity : Float
    , transition : Transition.Transition
    , specified : Bool
    }


type alias Property =
    { name : String
    , format : Format
    , defaults : List Float
    , channels : List Channel
    }


type alias State =
    Dict String Property


type alias Motion =
    { start : Float
    , duration : Float
    , from : State
    , to : State
    }


type alias Clip =
    { start : Float
    , duration : Float
    , iterations : Int
    , stop : Float
    , initial : State
    , final : State
    , motions : List Motion
    }


type alias Scene =
    { props : List Prop, steps : List Step }


type alias Event =
    { start : Float, arrival : Float, stop : Float, scene : Scene }


type alias Schedule =
    { time : Float, state : State, clips : List Clip }


forever : Float
forever =
    1 / 0


transition : Time.Duration -> List Prop -> Css
transition duration props =
    let
        initial =
            defaultsFor props

        target =
            resolve props initial

        clip =
            single 0 (milliseconds duration) forever initial target
    in
    render True 0 initial [ clip ]


keyframes : List Step -> Css
keyframes steps =
    let
        normalized =
            normalizeSteps steps

        initial =
            defaultsFor (stepProps normalized)

        scheduled =
            scheduleSteps forever normalized { time = 0, state = initial, clips = [] }
    in
    render False 0 initial (List.reverse scheduled.clips)


onTimeline : Timeline.Timeline state -> (state -> ( List Prop, List Step )) -> Css
onTimeline (Timeline.Timeline details) lookup =
    let
        scene state =
            let
                ( props, steps ) =
                    lookup state
            in
            { props = props, steps = normalizeSteps steps }

        events =
            case details.events of
                Timeline.Timetable lines ->
                    timelineEvents scene lines

        initialScene =
            scene details.initial

        defaults =
            initialScene
                :: List.map .scene events
                |> List.concatMap (\item -> item.props ++ stepProps item.steps)
                |> defaultsFor

        initial =
            resolve initialScene.props defaults

        origin =
            Time.inMilliseconds details.updatedAt - milliseconds details.delay

        firstStart =
            List.head events |> Maybe.map .start |> Maybe.withDefault origin

        initialStart =
            details.initialStartedAt
                |> Maybe.map Time.inMilliseconds
                |> Maybe.withDefault (Time.inMilliseconds details.updatedAt)

        resting =
            scheduleSteps
                (if List.isEmpty events then
                    forever

                 else
                    firstStart
                )
                initialScene.steps
                { time = initialStart, state = initial, clips = [] }

        scheduled =
            List.foldl
                (\event previous ->
                    let
                        target =
                            resolve event.scene.props defaults

                        clip =
                            single event.start (max 0 (event.arrival - event.start)) event.stop previous.state target

                        atArrival =
                            { time = event.arrival
                            , state = sampleClip (min event.stop event.arrival) clip
                            , clips = clip :: previous.clips
                            }
                    in
                    if event.stop < event.arrival then
                        atArrival

                    else
                        scheduleSteps event.stop event.scene.steps atArrival
                )
                resting
                events
    in
    render False origin initial (List.reverse scheduled.clips)


timelineEvents : (state -> Scene) -> List (Timeline.Line state) -> List Event
timelineEvents lookup lines =
    timelineEventsHelp lookup lines []


timelineEventsHelp : (state -> Scene) -> List (Timeline.Line state) -> List Event -> List Event
timelineEventsHelp lookup lines collected =
    case lines of
        [] ->
            List.reverse collected

        (Timeline.Line start first rest) :: following ->
            let
                stop =
                    case following of
                        (Timeline.Line nextStart _ _) :: _ ->
                            Time.inMilliseconds nextStart

                        [] ->
                            forever
            in
            timelineEventsHelp lookup
                following
                (lineEvents lookup stop (Time.inMilliseconds start) (first :: rest) collected)


lineEvents : (state -> Scene) -> Float -> Float -> List (Timeline.Occurring state) -> List Event -> List Event
lineEvents lookup cutoff start events collected =
    case events of
        [] ->
            collected

        event :: rest ->
            if start > cutoff then
                collected

            else
                let
                    nextStart =
                        Time.inMilliseconds (Timeline.endTime event)
                in
                lineEvents lookup
                    cutoff
                    nextStart
                    rest
                    ({ start = start
                     , arrival = Time.inMilliseconds (Timeline.startTime event)
                     , stop =
                        if List.isEmpty rest then
                            cutoff

                        else
                            min cutoff nextStart
                     , scene = lookup (Timeline.getEvent event)
                     }
                        :: collected
                    )


single : Float -> Float -> Float -> State -> State -> Clip
single start duration stop from to =
    { start = start
    , duration = duration
    , iterations = 1
    , stop = stop
    , initial = from
    , final = to
    , motions = [ { start = 0, duration = duration, from = from, to = to } ]
    }


{-| A run-once sequence is only a grouping, not a new CSS iteration. Flatten it
before compiling cycles. A repeat whose entire body is another repeat can also
keep its repetition in CSS instead of materializing one copy of every frame.
-}
normalizeSteps : List Step -> List Step
normalizeSteps steps =
    List.concatMap
        (\step ->
            case step of
                Step duration props ->
                    if milliseconds duration == 0 && List.isEmpty props then
                        []

                    else
                        [ step ]

                Repeat count children ->
                    if count == 0 then
                        []

                    else if count == 1 then
                        normalizeSteps children

                    else
                        case normalizeSteps children of
                            [] ->
                                []

                            [ Repeat innerCount innerSteps ] ->
                                [ Repeat
                                    (if count < 0 || innerCount < 0 then
                                        -1

                                     else
                                        count * innerCount
                                    )
                                    innerSteps
                                ]

                            normalized ->
                                [ Repeat count normalized ]
        )
        steps


scheduleSteps : Float -> List Step -> Schedule -> Schedule
scheduleSteps stop steps scheduled =
    if scheduled.time >= stop then
        scheduled

    else
        case steps of
            [] ->
                scheduled

            (Step duration props) :: rest ->
                let
                    clip =
                        single scheduled.time (milliseconds duration) stop scheduled.state (resolve props scheduled.state)
                in
                continueSteps stop rest clip scheduled

            (Repeat count children) :: rest ->
                if count == 0 || List.isEmpty children then
                    scheduleSteps stop rest scheduled

                else if List.any isInfiniteStep children then
                    -- An infinite child prevents its enclosing sequence from
                    -- ever completing, so the outer repeat cannot restart.
                    scheduleSteps stop children scheduled

                else
                    let
                        cycle =
                            cycleMotions children { time = 0, state = scheduled.state, motions = [] }

                        clip =
                            { start = scheduled.time
                            , duration = cycle.time
                            , iterations = count
                            , stop = stop
                            , initial = scheduled.state
                            , final = cycle.state
                            , motions = List.reverse cycle.motions
                            }
                    in
                    continueSteps stop rest clip scheduled


continueSteps : Float -> List Step -> Clip -> Schedule -> Schedule
continueSteps stop rest clip scheduled =
    let
        end =
            clipEnd clip

        next =
            { time = end
            , state = sampleClip (min stop end) clip
            , clips = clip :: scheduled.clips
            }
    in
    scheduleSteps stop rest next


isInfiniteStep : Step -> Bool
isInfiniteStep step =
    isInfinite (stepDuration step)


stepDuration : Step -> Float
stepDuration step =
    case step of
        Step duration _ ->
            milliseconds duration

        Repeat count children ->
            let
                duration =
                    List.sum (List.map stepDuration children)
            in
            if count == 0 || duration == 0 then
                0

            else if count < 0 then
                forever

            else
                toFloat count * duration


type alias Cycle =
    { time : Float, state : State, motions : List Motion }


cycleMotions : List Step -> Cycle -> Cycle
cycleMotions steps initial =
    List.foldl
        (\step cycle ->
            case step of
                Step duration props ->
                    let
                        target =
                            resolve props cycle.state

                        length =
                            milliseconds duration
                    in
                    { time = cycle.time + length
                    , state = target
                    , motions = { start = cycle.time, duration = length, from = cycle.state, to = target } :: cycle.motions
                    }

                Repeat count children ->
                    repeatCycle count children cycle
        )
        initial
        steps


repeatCycle : Int -> List Step -> Cycle -> Cycle
repeatCycle count steps cycle =
    if count <= 0 then
        cycle

    else
        let
            one =
                cycleMotions steps { time = 0, state = cycle.state, motions = [] }
        in
        { time = cycle.time + toFloat count * one.time
        , state = one.state
        , motions = repeatMotions count cycle.time one.time one.motions cycle.motions
        }


repeatMotions : Int -> Float -> Float -> List Motion -> List Motion -> List Motion
repeatMotions count start duration motions collected =
    if count <= 0 then
        collected

    else
        repeatMotions (count - 1)
            (start + duration)
            duration
            motions
            (List.map (\motion -> { motion | start = motion.start + start }) motions ++ collected)


clipEnd : Clip -> Float
clipEnd clip =
    if clip.duration == 0 then
        clip.start

    else if clip.iterations < 0 then
        forever

    else
        clip.start + clip.duration * toFloat clip.iterations


sampleClip : Float -> Clip -> State
sampleClip now clip =
    if now < clip.start then
        clip.initial

    else if clip.duration == 0 || now >= clipEnd clip then
        settle clip.final

    else
        let
            elapsed =
                now - clip.start

            local =
                elapsed - toFloat (floor (elapsed / clip.duration)) * clip.duration
        in
        List.foldl
            (\motion state ->
                if local < motion.start then
                    state

                else
                    sampleMotion local motion
            )
            clip.initial
            clip.motions


sampleMotion : Float -> Motion -> State
sampleMotion now motion =
    if motion.duration == 0 || now >= motion.start + motion.duration then
        settle motion.to

    else
        Dict.map
            (\name target ->
                let
                    from =
                        Dict.get name motion.from |> Maybe.withDefault target
                in
                { target
                    | channels =
                        List.map2
                            (sampleChannel ((now - motion.start) / motion.duration) motion.duration)
                            from.channels
                            target.channels
                }
            )
            motion.to


sampleChannel : Float -> Float -> Channel -> Channel -> Channel
sampleChannel progress duration from target =
    let
        sampled =
            Transition.atX (clamp 0 1 progress)
                (Time.millis 0)
                (Time.millis duration)
                target.transition
                { position = Units.pixels from.position, velocity = Units.pixelsPerSecond from.velocity }
                target.position
    in
    { target | position = Units.inPixels sampled.position, velocity = Units.inPixelsPerSecond sampled.velocity }


settle : State -> State
settle =
    Dict.map (\_ prop -> { prop | channels = List.map (\part -> { part | velocity = 0 }) prop.channels })


type alias Output =
    { base : State
    , animations : List String
    , keyframes : List String
    , transitions : List String
    , animated : Dict String Bool
    }


render : Bool -> Float -> State -> List Clip -> Css
render allowTransitions origin initial clips =
    let
        output =
            List.foldl
                (renderClip allowTransitions origin)
                { base = initial, animations = [], keyframes = [], transitions = [], animated = Dict.empty }
                clips

        transitionCss =
            String.join ", " (List.reverse output.transitions)

        animationCss =
            String.join ", " (List.reverse output.animations)

        props =
            Dict.values output.base |> List.map (\prop -> ( prop.name, format prop ))

        animationProps =
            case output.animations of
                [] ->
                    []

                _ ->
                    [ ( "animation", animationCss ) ]

        transitionProps =
            if transitionCss == "" then
                []

            else
                [ ( "transition", transitionCss ) ]

        keyframeCss =
            String.join "\n" (List.reverse output.keyframes)
    in
    { hash = hash (keyframeCss ++ animationCss ++ transitionCss ++ String.join ";" (List.map (\( name, value ) -> name ++ ":" ++ value) props))
    , keyframes = keyframeCss
    , transition = transitionCss
    , props = animationProps ++ transitionProps ++ props
    }


renderClip : Bool -> Float -> Clip -> Output -> Output
renderClip allowTransitions origin clip output =
    let
        atOrigin =
            if clip.start <= origin then
                { output | base = sampleClip (min origin clip.stop) clip }

            else
                output
    in
    if clip.stop <= origin || (clipEnd clip <= origin && clip.duration > 0) then
        atOrigin

    else
        Dict.foldl
            (\name initial result ->
                let
                    target =
                        Dict.get name clip.final |> Maybe.withDefault initial

                    native =
                        if allowTransitions && clip.iterations == 1 && List.length clip.motions == 1 then
                            nativeCurve target
                                |> Maybe.map (Css.easing (Duration.milliseconds clip.duration))

                        else
                            Nothing

                    changed =
                        List.any
                            (\motion ->
                                propertyChanges name motion
                            )
                            clip.motions
                in
                case native of
                    Just easing ->
                        { result
                            | base = Dict.insert name target result.base
                            , transitions =
                                if clip.duration == 0 then
                                    result.transitions

                                else
                                    (name ++ " " ++ ms clip.duration ++ " " ++ easing ++ " " ++ ms (clip.start - origin)) :: result.transitions
                        }

                    Nothing ->
                        if not changed && not (Dict.member name result.animated) then
                            result

                        else if clip.duration == 0 && clip.start <= origin then
                            { result | base = Dict.insert name target result.base }

                        else
                            let
                                frames =
                                    framesForMotions name initial clip.duration clip.motions

                                animationName =
                                    "anim-" ++ hash (String.join ":" [ name, frames, String.fromFloat origin, String.fromFloat clip.start, String.fromFloat clip.duration, String.fromInt clip.iterations ])

                                animation =
                                    Css.animation (Duration.milliseconds clip.duration)
                                        (Duration.milliseconds (clip.start - origin))
                                        clip.iterations
                                        animationName
                            in
                            { result
                                | animations = animation :: result.animations
                                , keyframes = Css.keyframes animationName frames :: result.keyframes
                                , animated = Dict.insert name True result.animated
                            }
            )
            atOrigin
            clip.initial


propertyChanges : String -> Motion -> Bool
propertyChanges name motion =
    case ( Dict.get name motion.from, Dict.get name motion.to ) of
        ( Just from, Just to ) ->
            values from /= values to || from.format /= to.format || List.any (\part -> part.velocity /= 0) from.channels

        _ ->
            False


nativeCurve : Property -> Maybe Transition.Transition
nativeCurve property =
    -- Native transitions start at the browser's current value, not at our
    -- defaults. Select curves from explicitly supplied channels, even when
    -- their targets equal those defaults (e.g. a spring returning to x = 0).
    case List.filter .specified property.channels |> List.map .transition of
        first :: rest ->
            if List.all ((==) first) rest then
                Just first

            else
                Nothing

        [] ->
            Nothing


commonBezier : Property -> Property -> Maybe Bezier.Spline
commonBezier from to =
    let
        changed =
            List.map2 (\one two -> ( one.position /= two.position, two.transition )) from.channels to.channels
                |> List.filter Tuple.first
                |> List.map Tuple.second

        transitions =
            if List.isEmpty changed then
                List.map .transition to.channels

            else
                changed
    in
    case transitions of
        ((Transition.Transition spline) as first) :: rest ->
            if List.all ((==) first) rest then
                Just spline

            else
                Nothing

        _ ->
            Nothing


framesForMotions : String -> Property -> Float -> List Motion -> String
framesForMotions name fallback total motions =
    framesForMotionsHelp name fallback total motions []


framesForMotionsHelp : String -> Property -> Float -> List Motion -> List String -> String
framesForMotionsHelp name fallback total motions collected =
    case motions of
        [] ->
            String.concat (List.reverse collected)

        motion :: rest ->
            let
                rendered =
                    case rest of
                        next :: _ ->
                            let
                                position state =
                                    Dict.get name state |> Maybe.map (\prop -> ( prop.format, values prop ))

                                nextPosition =
                                    if next.duration == 0 then
                                        position next.to

                                    else
                                        position next.from
                            in
                            if motion.duration > 0 && motion.start + motion.duration == next.start && position motion.to /= nextPosition then
                                -- CSS merges duplicate offsets. Preserve the
                                -- endpoint just before an instantaneous set or
                                -- a nested repeat resets its starting value.
                                { motion | duration = motion.duration - min 0.001 (motion.duration / 1000) }

                            else
                                motion

                        [] ->
                            motion
            in
            framesForMotionsHelp name fallback total rest (motionFrames name fallback total rendered :: collected)


motionFrames : String -> Property -> Float -> Motion -> String
motionFrames name fallback total motion =
    let
        from =
            Dict.get name motion.from |> Maybe.withDefault fallback

        to =
            Dict.get name motion.to |> Maybe.withDefault fallback

        frame elapsed property easing =
            Css.frame
                (if total == 0 then
                    100

                 else
                    100 * elapsed / total
                )
                (Css.prop name (format property) ++ easing)
    in
    if motion.duration == 0 then
        frame motion.start to ""

    else
        case commonBezier from to of
            Just spline ->
                frame motion.start from (Css.timingFunction spline)
                    ++ frame (motion.start + motion.duration) to ""

            Nothing ->
                sampledFrames frame motion from to


sampledFrames : (Float -> Property -> String -> String) -> Motion -> Property -> Property -> String
sampledFrames frame motion from to =
    -- Springs are not single CSS timing functions, and compound properties
    -- cannot assign different timing functions to individual axes. Sample
    -- those cases in the actual time/value domain, with a bounded frame count.
    -- Sample the requested duration, which need not equal the spring's
    -- estimated settling time, and explicitly land on the destination.
    let
        count =
            clamp 2 240 (ceiling (motion.duration / (1000 / 60)))
    in
    List.range 0 count
        |> List.map
            (\index ->
                let
                    time =
                        motion.start + motion.duration * toFloat index / toFloat count
                in
                frame time
                    (if index == count then
                        to

                     else
                        { to | channels = List.map2 (sampleChannel (toFloat index / toFloat count) motion.duration) from.channels to.channels }
                    )
                    "animation-timing-function:linear;"
            )
        |> String.concat


defaultsFor : List Prop -> State
defaultsFor props =
    resolve props Dict.empty
        |> Dict.map (\_ property -> { property | channels = List.map channel property.defaults })


resolve : List Prop -> State -> State
resolve props initial =
    List.foldl resolveProp initial props


resolveProp : Prop -> State -> State
resolveProp prop state =
    case prop of
        Prop id name movement scalarFormat ->
            let
                target =
                    { position = Move.toValue movement, velocity = 0, transition = Move.toTransition movement, specified = True }
            in
            if Props.isTranslateId id || Props.isScaleId id then
                let
                    scaling =
                        Props.isScaleId id

                    default =
                        if scaling then
                            1

                        else
                            0

                    empty =
                        { name = name
                        , format =
                            if scaling then
                                Scaling

                            else
                                Translation
                        , defaults = List.repeat 3 default
                        , channels = List.repeat 3 (channel default)
                        }

                    previous =
                        Dict.get name state |> Maybe.withDefault empty

                    axis =
                        if scaling then
                            id - Props.ids.scaleX

                        else
                            id
                in
                Dict.insert name
                    { empty
                        | channels =
                            List.indexedMap
                                (\index current ->
                                    if id == Props.ids.scale || index == axis then
                                        target

                                    else
                                        current
                                )
                                previous.channels
                    }
                    state

            else
                Dict.insert name
                    { name = name, format = Scalar scalarFormat, defaults = [ Props.defaultPosition id ], channels = [ target ] }
                    state

        ColorProp name movement ->
            let
                rgba =
                    Color.toRgba (Move.toValue movement)

                trans =
                    Move.toTransition movement
            in
            Dict.insert name
                { name = name
                , format = Rgba
                , defaults = [ 0, 0, 0, 0 ]
                , channels = List.map (\value -> { position = value, velocity = 0, transition = trans, specified = True }) [ rgba.red, rgba.green, rgba.blue, rgba.alpha ]
                }
                state


channel : Float -> Channel
channel value =
    { position = value, velocity = 0, transition = Transition.standard, specified = False }


values : Property -> List Float
values =
    .channels >> List.map .position


format : Property -> String
format property =
    case ( property.format, values property ) of
        ( Scalar scalarFormat, value :: _ ) ->
            Props.format scalarFormat value

        ( Translation, [ x, y, z ] ) ->
            Props.vectorToString Props.groups.translation { x = x, y = y, z = z }

        ( Scaling, [ x, y, z ] ) ->
            Props.vectorToString Props.groups.scaling { x = x, y = y, z = z }

        ( Rgba, [ red, green, blue, alpha ] ) ->
            Color.toCssString (Color.rgba red green blue alpha)

        _ ->
            ""


stepProps : List Step -> List Prop
stepProps =
    List.concatMap
        (\step ->
            case step of
                Step _ props ->
                    props

                Repeat count children ->
                    if count == 0 then
                        []

                    else
                        stepProps children
        )


milliseconds : Time.Duration -> Float
milliseconds =
    Duration.inMilliseconds >> max 0


ms : Float -> String
ms value =
    String.fromFloat value ++ "ms"


hash : String -> String
hash source =
    String.foldl (\char value -> Bitwise.or 0 (value * 31 + Char.toCode char)) 5381 source
        |> String.fromInt
