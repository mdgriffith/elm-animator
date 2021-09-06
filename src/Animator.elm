module Animator exposing
    ( Animation, delay, transition
    , Attribute, opacity
    , rotation, x, y, scale, scaleX, scaleY
    , color, px, int, float
    , withWobble, withBezier, withImpulse
    , Duration, ms
    , spinning, pulsing, bouncing, pinging
    , keyframes, loop, loopFor
    , set, wait, step
    , onTimeline, onTimelineWith
    , div, node
    , Css, css
    , xAsSingleProp
    )

{-|

@docs Animation, delay, transition

@docs Attribute, opacity

@docs rotation, x, y, scale, scaleX, scaleY

@docs color, px, int, float

@docs withWobble, withBezier, withImpulse

@docs Duration, ms


# Premade

Here are some premade animations.

There's nothing special about them, they're just convenient!

Check out how they're defined if you want to make your own.

@docs spinning, pulsing, bouncing, pinging


# Sequences

@docs keyframes, loop, loopFor

@docs set, wait, step

@docs onTimeline, onTimelineWith

@docs delay


# Rendering

@docs div, node

@docs Css, css

-}

import Animator.Timeline exposing (Timeline)
import Color
import Duration
import Html exposing (Html)
import Html.Attributes as Attr
import Internal.Css as Css
import Internal.Css.Props
import Internal.Move as Move
import Internal.Time as Time
import Internal.Timeline as Timeline
import Quantity
import Time


{-| -}
type alias Attribute =
    Css.Prop


{-| -}
opacity : Float -> Attribute
opacity o =
    Css.Prop
        Internal.Css.Props.ids.opacity
        "opacity"
        (Move.to o)
        Internal.Css.Props.float


{-| -}
xAsSingleProp : Float -> Attribute
xAsSingleProp o =
    Css.Prop
        Internal.Css.Props.ids.opacity
        "transform"
        (Move.to o)
        Internal.Css.Props.translateX


{-| -}
scale : Float -> Attribute
scale s =
    Css.Prop
        Internal.Css.Props.ids.scale
        ""
        (Move.to s)
        Internal.Css.Props.float


{-| -}
scaleX : Float -> Attribute
scaleX s =
    Css.Prop
        Internal.Css.Props.ids.scaleX
        ""
        (Move.to s)
        Internal.Css.Props.float


{-| -}
scaleY : Float -> Attribute
scaleY s =
    Css.Prop
        Internal.Css.Props.ids.scaleY
        ""
        (Move.to s)
        Internal.Css.Props.float


{-| -}
rotation : Float -> Attribute
rotation n =
    Css.Prop
        Internal.Css.Props.ids.rotation
        ""
        (Move.to n)
        Internal.Css.Props.float


{-| -}
x : Float -> Attribute
x n =
    Css.Prop
        Internal.Css.Props.ids.x
        ""
        (Move.to n)
        Internal.Css.Props.float


{-| -}
y : Float -> Attribute
y n =
    Css.Prop
        Internal.Css.Props.ids.y
        ""
        (Move.to n)
        Internal.Css.Props.float


{-| -}
withBezier : Float -> Float -> Float -> Float -> Attribute -> Attribute
withBezier one two three four prop =
    case prop of
        Css.Prop id name move format ->
            Css.Prop id name (Move.withBezier one two three four move) format

        Css.ColorProp name move ->
            Css.ColorProp name (Move.withBezier one two three four move)


{-| -}
withWobble : Float -> Attribute -> Attribute
withWobble wob prop =
    case prop of
        Css.Prop id name move format ->
            Css.Prop id name (Move.withWobble wob move) format

        Css.ColorProp name move ->
            Css.ColorProp name (Move.withWobble wob move)


type alias Duration =
    Time.Duration


{-| -}
ms : Float -> Duration
ms =
    Duration.milliseconds


{-| When transitioning to this state, start with a little extra velocity!

**Values**
0 -> No different from before
1 ->

-}
withImpulse : Float -> Attribute -> Attribute
withImpulse impulse prop =
    case prop of
        Css.Prop id name move format ->
            Css.Prop id name (Move.withVelocities impulse 0 move) format

        Css.ColorProp name move ->
            Css.ColorProp name (Move.withVelocities impulse 0 move)


{-| -}
delay : Duration -> Animation -> Animation
delay dur (Animation now attrs) =
    Animation (Time.rollbackBy dur now) attrs


type Animation
    = Animation Time.Absolute (List Css.RenderedProp)


type Step
    = Step Duration (List Attribute)


set : List Attribute -> Step
set attrs =
    step Time.zeroDuration attrs


wait : Duration -> Step
wait dur =
    step dur []


step : Duration -> List Attribute -> Step
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


toOccurring : Time.Absolute -> Step -> ( Time.Absolute, Timeline.Occurring (List Attribute) )
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


getInitialProps : Time.Duration -> List Step -> List Attribute -> List Attribute
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
    -> List Attribute
    -> List Attribute
    -> List Attribute
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


addSequence : Int -> List Step -> List Attribute -> Step
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


addSequenceSteps : Int -> Time.Duration -> List Step -> Attribute -> Attribute
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
    -> Attribute
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
    -> Attribute
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


firstMatch : Attribute -> List Attribute -> Maybe Attribute
firstMatch prop props =
    case props of
        [] ->
            Nothing

        next :: remain ->
            if Css.match prop next then
                Just next

            else
                firstMatch prop remain


{-| -}
px : String -> Float -> Attribute
px name n =
    Css.Prop
        Internal.Css.Props.noId
        name
        (Move.to n)
        Internal.Css.Props.px


{-| -}
int : String -> Float -> Attribute
int name n =
    Css.Prop
        Internal.Css.Props.noId
        name
        (Move.to n)
        Internal.Css.Props.int


{-| -}
float : String -> Float -> Attribute
float name n =
    Css.Prop
        Internal.Css.Props.noId
        name
        (Move.to n)
        Internal.Css.Props.float


{-| -}
color : String -> Color.Color -> Attribute
color name colorValue =
    Css.ColorProp name
        (Move.to colorValue)


spinning : Duration -> Animation
spinning dur =
    keyframes
        [ loop
            [ set
                [ rotation 0
                ]
            , step dur
                [ rotation 1
                ]
            ]
        ]


pulsing : Duration -> Animation
pulsing dur =
    keyframes
        [ loop
            [ set
                [ opacity 1
                ]
            , step dur
                [ opacity 0.5
                ]
            ]
        ]


bouncing : Duration -> Float -> Animation
bouncing dur distance =
    if Time.isZeroDuration dur then
        keyframes []

    else
        let
            half =
                dur |> Quantity.divideBy 2

            startingY =
                y 0
                    |> withBezier 0.8 0 1 1
        in
        keyframes
            [ loop
                [ set
                    [ startingY ]
                , step half
                    [ y distance
                        |> withBezier 0 0 0.2 1
                    ]
                , step half
                    [ startingY
                    ]
                ]
            ]


pinging : Duration -> Animation
pinging dur =
    keyframes
        [ loop
            [ set
                [ rotation 0
                , opacity 1
                ]
            , step dur
                [ rotation 1
                , opacity 0
                ]
            ]
        ]


onTimeline : Timeline state -> (state -> List Attribute) -> Animation
onTimeline timeline toProps =
    Animation
        (Timeline.getCurrentTime timeline)
        (Css.propsToRenderedProps timeline toProps)


onTimelineWith :
    Timeline state
    ->
        (state
         -> ( List Attribute, List Step )
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
transition : Animator.Timeline.Duration -> List Attribute -> Animation
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
div (Animation now renderedProps) attrs children =
    let
        rendered =
            Css.toCss now renderedProps

        styles =
            List.map (\( propName, val ) -> Attr.style propName val)
                (( "animation", rendered.animation ) :: rendered.props)
    in
    Html.div
        (styles ++ attrs)
        (stylesheet rendered.keyframes
            :: children
        )


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
            Css.toCss now renderedProps

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
css : Timeline state -> (state -> List Attribute) -> Css
css =
    Css.cssFromProps


{-| -}
stylesheet : String -> Html msg
stylesheet str =
    case str of
        "" ->
            Html.text ""

        _ ->
            Html.node "style"
                []
                [ Html.text str
                ]
