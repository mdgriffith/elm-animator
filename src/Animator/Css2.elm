module Animator.Css2 exposing
    ( opacity
    , rotation, x, y, scale, scaleX, scaleY
    , color, px, int, float
    , withWobble
    , keyframes, loop, loopFor
    , set, wait, step
    , onTimeline, onTimelineWith
    , div, node
    , Css, css
    , watching
    , xAsSingleProp
    )

{-|

@docs Property

@docs opacity

@docs rotation, x, y, scale, scaleX, scaleY

@docs color, px, int, float

@docs withWobble


# Sequences

@docs keyframes, loop, loopFor

@docs set, wait, step

@docs onTimeline, onTimelineWith


# Rendering

@docs div, node

@docs Css, css

@docs watching

-}

import Animator exposing (Animator, Timeline)
import Color
import Duration
import Html exposing (Html)
import Html.Attributes as Attr
import Internal.Css as Css
import Internal.Css.Props
import Internal.Move as Move
import Internal.Time as Time
import Internal.Timeline as Timeline
import Time


{-| -}
type alias Property =
    Css.Prop


{-| opacity : Movement -> Property

    opacity (at 0)

    Animated.opacity (Animated.at 0)

    opacity
        (loop
            (ms 300)
            (wave 0 1)
        )

    Animated.opacity
        (Animated.loop
            Animated.quickly
            (Animated.wave 0 1)
        )

    opacity <|
        loop
            (ms 300)
            (wave 0 1)

    loop
        (ms 300)
        (wave 0 1)
        |> opacity

OR

opacity : Float -> Property
where
loop : Duration -> Oscillation -> Property -> Proeprty

    opacity 0

    opacity 0
        |> withWobble 1

    opacity 0
        |> withLeaveSmoothly 0.2

    Animated.opacity 0
        |> loop (ms 300)
            (wave 0 1)

    Anim.div
        [ loop (ms 300)
            [ group
                [ opacity 0
                ]
            , group
                [ opacity 1
                ]
            ]
        ]

-}
opacity : Float -> Property
opacity o =
    Css.Prop
        Internal.Css.Props.ids.opacity
        "opacity"
        (Move.to o)
        Internal.Css.Props.float


xAsSingleProp : Float -> Property
xAsSingleProp o =
    Css.Prop
        Internal.Css.Props.ids.opacity
        "transform"
        (Move.to o)
        Internal.Css.Props.translateX


{-| -}
scale : Float -> Property
scale s =
    Css.Prop
        Internal.Css.Props.ids.scale
        ""
        (Move.to s)
        Internal.Css.Props.float


{-| -}
scaleX : Float -> Property
scaleX s =
    Css.Prop
        Internal.Css.Props.ids.scaleX
        ""
        (Move.to s)
        Internal.Css.Props.float


{-| -}
scaleY : Float -> Property
scaleY s =
    Css.Prop
        Internal.Css.Props.ids.scaleY
        ""
        (Move.to s)
        Internal.Css.Props.float


{-| -}
rotation : Float -> Property
rotation n =
    Css.Prop
        Internal.Css.Props.ids.rotation
        ""
        (Move.to n)
        Internal.Css.Props.float


{-| -}
x : Float -> Property
x n =
    Css.Prop
        Internal.Css.Props.ids.x
        ""
        (Move.to n)
        Internal.Css.Props.float


{-| -}
y : Float -> Property
y n =
    Css.Prop
        Internal.Css.Props.ids.y
        ""
        (Move.to n)
        Internal.Css.Props.float


{-| -}
withWobble : Float -> Property -> Property
withWobble wob prop =
    case prop of
        Css.Prop id name move format ->
            Css.Prop id name (Move.withWobble wob move) format

        Css.ColorProp name move ->
            Css.ColorProp name (Move.withWobble wob move)



-- {-|
-- Transition to this state
-- -}
-- withImpulse : Float -> Property -> Property
-- withImpulse impulse prop =
--     case prop of
--         Css.Prop id name move format ->
--             Css.Prop id name (Move.withImpulse impulse move) format
--         Css.ColorProp name move ->
--             Css.ColorProp name (Move.withImpulse impulse move)


{-| -}
withDelay : Animator.Duration -> Property -> Property
withDelay dur p =
    case p of
        Css.Prop id name move format ->
            Css.Prop id name (Move.withDelay dur move) format

        Css.ColorProp name move ->
            Css.ColorProp name (Move.withDelay dur move)



-- {-| -}
-- withCurve : Bezier.Spline -> Property -> Property
-- withCurve spline p =
--     p
{- SEQUENCER API



   It can be really useful to define a sequence for an animation inline.


   The basic constructors are
   - sequence -> procede through a list of steps
   - repeat : Int -> repeat list of steps X number of times
   - repeatForever : repeat forever


   Things to avoid
   1. nested constructors


   -- normally there should be a timeline
   Anim.onTimeline timeline
       (\state ->
           [ opacity 0
           , x 5
           ]
       ) : Animation


   -- Though for simple cases, we don't have an explicit timeline.




   Anim.sequence
       [ step (ms 300)
           [ opacity 0
           ]
       , step (ms 200)
           [ opacity 1
           ]
       ]
       : Animation



   Anim.repeat 5
       [ step (ms 300)
           [ opacity 0
           ]
       , set
           [ opacity 1
           ]
       ]
       : Animation








-}


type Animation
    = Animation Time.Absolute (List Css.RenderedProp)


type alias Attribute =
    List Property


type Step
    = Step Animator.Duration (List Property)


set : List Property -> Step
set attrs =
    step Time.zeroDuration attrs


wait : Animator.Duration -> Step
wait dur =
    step dur []


step : Animator.Duration -> List Property -> Step
step =
    Step


{-| -}
keyframes : List Step -> Animation
keyframes steps =
    let
        imminent =
            Time.absolute (Time.millisToPosix 1)

        ( ( afterFirst, firstOccurring ), remaining ) =
            case steps of
                [] ->
                    ( ( imminent, Timeline.Occurring [] imminent imminent ), [] )

                first :: r ->
                    ( toOccurring imminent first, r )

        timeline =
            Timeline.Timeline
                { initial = []
                , now = imminent
                , delay = Time.zeroDuration
                , scale = 1
                , events =
                    Timeline.Timetable
                        [ Timeline.Line
                            imminent
                            firstOccurring
                            (List.foldl
                                (\currentStep ( time, occurs ) ->
                                    let
                                        ( newTime, occur ) =
                                            toOccurring time currentStep
                                    in
                                    ( newTime, occur :: occurs )
                                )
                                ( afterFirst, [] )
                                remaining
                                |> Tuple.second
                            )
                        ]
                , queued = Nothing
                , interruption = []
                , running = True
                }
    in
    Animation
        (Timeline.getCurrentTime timeline)
        (Css.propsToRenderedProps timeline identity)


toOccurring : Time.Absolute -> Step -> ( Time.Absolute, Timeline.Occurring (List Property) )
toOccurring currentTime (Step dur props) =
    let
        time =
            Time.advanceBy dur currentTime
    in
    ( time, Timeline.Occurring props time time )


{-| -}
loop : List Step -> Step
loop steps =
    -- abusing infinite here :/ don't look at me!
    loopFor (1 // 0) steps


{-| -}
loopFor : Int -> List Step -> Step
loopFor n steps =
    let
        initialProps =
            getInitialProps Time.zeroDuration steps []
    in
    addSequence n
        steps
        initialProps


getInitialProps : Time.Duration -> List Step -> List Property -> List Property
getInitialProps durationTillThisStep steps props =
    case steps of
        [] ->
            props

        (Step dur stepProps) :: remaining ->
            let
                newProps =
                    addIfNew durationTillThisStep stepProps props
            in
            getInitialProps (Time.expand durationTillThisStep dur)
                remaining
                newProps


addIfNew :
    Time.Duration
    -> List Property
    -> List Property
    -> List Property
addIfNew durationTillThisStep stepProps props =
    case props of
        [] ->
            stepProps

        _ ->
            case stepProps of
                [] ->
                    props

                topStep :: remainingSteps ->
                    if List.any (Css.match topStep) props then
                        addIfNew durationTillThisStep remainingSteps props

                    else
                        addIfNew durationTillThisStep
                            remainingSteps
                            -- Note::  we aren't doing anything special for defaults here
                            -- However, ultimately we could get clever and stub in a default from the node itself
                            (topStep :: props)


addSequence : Int -> List Step -> List Property -> Step
addSequence n steps prop =
    let
        fullDuration =
            sumStepDuration Time.zeroDuration steps
    in
    prop
        |> List.map (addSequenceSteps n fullDuration steps)
        |> Step fullDuration


sumStepDuration : Time.Duration -> List Step -> Time.Duration
sumStepDuration dur steps =
    case steps of
        [] ->
            dur

        (Step stepDur _) :: remain ->
            sumStepDuration (Time.expand stepDur dur) remain


addSequenceSteps : Int -> Time.Duration -> List Step -> Property -> Property
addSequenceSteps n fullDuration steps prop =
    case prop of
        Css.Prop id name movement format ->
            let
                formattedSteps =
                    formatSteps steps prop []
            in
            Css.Prop id
                name
                (Move.addSequence n fullDuration formattedSteps movement)
                format

        Css.ColorProp name movement ->
            let
                formattedSteps =
                    formatColorSteps steps prop []
            in
            Css.ColorProp name
                (Move.addSequence n fullDuration formattedSteps movement)


formatColorSteps :
    List Step
    -> Property
    -> List (Move.Step Color.Color)
    -> List (Move.Step Color.Color)
formatColorSteps steps prop pastSteps =
    case steps of
        [] ->
            List.reverse pastSteps

        (Step dur props) :: next ->
            case firstMatch prop props of
                Nothing ->
                    List.reverse pastSteps

                Just (Css.Prop id name _ format) ->
                    formatColorSteps next
                        prop
                        pastSteps

                Just (Css.ColorProp name (Move.Pos trans value _)) ->
                    formatColorSteps next
                        prop
                        (Move.stepWith dur trans value :: pastSteps)


formatSteps :
    List Step
    -> Property
    -> List (Move.Step Float)
    -> List (Move.Step Float)
formatSteps steps prop pastSteps =
    case steps of
        [] ->
            List.reverse pastSteps

        (Step dur props) :: next ->
            case firstMatch prop props of
                Nothing ->
                    List.reverse pastSteps

                Just (Css.Prop id name (Move.Pos trans value _) format) ->
                    formatSteps next
                        prop
                        (Move.stepWith dur trans value :: pastSteps)

                Just (Css.ColorProp name movement) ->
                    formatSteps next
                        prop
                        pastSteps


firstMatch : Property -> List Property -> Maybe Property
firstMatch prop props =
    case props of
        [] ->
            Nothing

        next :: remain ->
            if Css.match prop next then
                Just next

            else
                firstMatch prop remain



{-

   type Move value
       = Pos Transition.Transition value (List (Sequence value))

   type Sequence value
       = Sequence
           --  repeat
           Int
           -- delay
           Duration.Duration
           -- duration
           Duration.Duration
           (List (Step value))


   type Step value
       = Step Duration.Duration Transition.Transition value


    1. Create a list of properties based on the first step.
        - Step.duration -> transition duration
        -

    2. For each step
        if property is not found.
            !! track total duration till start of current step
            Add property to pool.
            Have it wait at a default value until the current step
            Step.duration -> transition duration

        otherwise

    3.  Prepare a single `Sequence` for each property



-}


{-| -}
px : String -> Float -> Property
px name n =
    Css.Prop
        Internal.Css.Props.noId
        name
        (Move.to n)
        Internal.Css.Props.px


{-| -}
int : String -> Float -> Property
int name n =
    Css.Prop
        Internal.Css.Props.noId
        name
        (Move.to n)
        Internal.Css.Props.int


{-| -}
float : String -> Float -> Property
float name n =
    Css.Prop
        Internal.Css.Props.noId
        name
        (Move.to n)
        Internal.Css.Props.float


{-| -}
color : String -> Color.Color -> Property
color name colorValue =
    Css.ColorProp name
        (Move.to colorValue)


onTimeline : Timeline state -> (state -> List Property) -> Animation
onTimeline timeline toProps =
    Animation
        (Timeline.getCurrentTime timeline)
        (Css.propsToRenderedProps timeline toProps)


onTimelineWith :
    Timeline state
    ->
        (state
         -> ( List Property, List Step )
        )
    -> Animation
onTimelineWith timeline toPropsAndSteps =
    let
        toProps event =
            let
                ( props, steps ) =
                    toPropsAndSteps event

                fullDuration =
                    sumStepDuration Time.zeroDuration steps
            in
            getInitialProps Time.zeroDuration steps props
                |> List.map (addSequenceSteps 1 fullDuration steps)
    in
    Animation
        (Timeline.getCurrentTime timeline)
        (Css.propsToRenderedProps timeline toProps)


{-| -}
transition : Animator.Duration -> List Property -> Animation
transition transitionDuration props =
    let
        imminent =
            Time.absolute (Time.millisToPosix 1)

        startTime =
            Time.advanceBy transitionDuration imminent

        timeline =
            Timeline.Timeline
                { initial = []
                , now = imminent
                , delay = Time.zeroDuration
                , scale = 1
                , events =
                    Timeline.Timetable
                        [ Timeline.Line
                            imminent
                            (Timeline.Occurring props startTime startTime)
                            []
                        ]
                , queued = Nothing
                , interruption = []
                , running = True
                }
    in
    Animation (Timeline.getCurrentTime timeline)
        (Css.propsToRenderedProps timeline identity)


{--}
div :
    Animation
    -> List (Html.Attribute msg)
    -> List (Html msg)
    -> Html msg
div =
    node "div"


{-| -}
node :
    String
    -> Animation
    -> List (Html.Attribute msg)
    -> List (Html msg)
    -> Html msg
node name (Animation now renderedProps) attrs children =
    let
        rendered =
            Debug.log "STYLE" (Css.toCss now renderedProps)

        styles =
            List.map (\( propName, val ) -> Attr.style propName val)
                (( "animation", rendered.animation ) :: rendered.props)
    in
    Html.node name
        (styles ++ attrs)
        (stylesheet rendered.keyframes
            :: children
        )


{-| -}
type alias Css =
    { hash : String

    -- use single prop encoding:
    -- https://developer.mozilla.org/en-US/docs/Web/CSS/animation
    , animation : String
    , keyframes : String
    , props : List ( String, String )
    }


{-| -}
css : Timeline state -> (state -> List Property) -> Css
css =
    Css.cssFromProps


{-| -}
stylesheet : String -> Html msg
stylesheet str =
    Html.node "style"
        []
        [ Html.text str
        ]


{--}
{- ANIMATOR -}


{-| `Animator.Css.watching` is different from `Animator.watching` in that it will only ask for one frame when an animation is updated.

In that one frame, we render the **entire CSS animation**, which can run without `Elm` needing to do a full rerender.

-}
watching :
    (model -> Timeline state)
    -> (Timeline state -> model -> model)
    -> Animator model
    -> Animator model
watching get setValue (Timeline.Animator isRunning updateModel) =
    Timeline.Animator
        (\model ->
            let
                prev =
                    isRunning model

                timeline =
                    get model

                ping =
                    case Timeline.sendPing timeline of
                        Nothing ->
                            prev.ping

                        Just currentPing ->
                            case prev.ping of
                                Nothing ->
                                    Just currentPing

                                Just prevPing ->
                                    if prevPing.delay < currentPing.delay then
                                        Just prevPing

                                    else
                                        Just currentPing

                running =
                    if prev.running then
                        True

                    else
                        Timeline.hasChanged timeline
                            || Timeline.justInitialized timeline
            in
            { running = running
            , ping = ping
            }
        )
        (\now model ->
            let
                newModel =
                    updateModel now model
            in
            setValue (Timeline.update now (get newModel)) newModel
        )
