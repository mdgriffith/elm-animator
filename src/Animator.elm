module Animator exposing
    ( Animation, transition
    , Attribute, opacity
    , rotation, rotationAround
    , x, y, z
    , scale, scaleX, scaleY, scaleZ
    , color, px, int, float
    , withTransition, withStepTransition
    , Duration, ms, delay
    , spinning, pulsing, bouncing, pinging
    , Step, set, wait, step
    , keyframes, loop, loopFor, sequence
    , onTimeline, onTimelineWith
    , div, node
    , Css, css, toCss
    )

{-|

@docs Animation, transition

@docs Attribute, opacity

@docs rotation, rotationAround

@docs x, y, z

@docs scale, scaleX, scaleY, scaleZ

@docs color, px, int, float

@docs withTransition, withStepTransition

@docs Duration, ms, delay


# Premade

Here are some premade animations.

There's nothing special about them, they're just convenient!

Check out how they're defined if you want to make your own.

@docs spinning, pulsing, bouncing, pinging


# Sequences

You may want something more involved than a single step transition.

Here's an element that's blinking.

    import Animator as Anim
    import Html
    import Html.Attributes


    Anim.div
        (Anim.loop
            [ Anim.step (Anim.ms 200)
                [ Anim.opacity 1
                ]
            , Anim.wait (Anim.ms 200)
            , Anim.step (Anim.ms 200)
                [ Anim.opacity 0
                ]
            ]
        )
        [ Html.Attributes.id "my-element" ]
        [ Html.text "Hello!" ]

@docs Step, set, wait, step

@docs keyframes, loop, loopFor, sequence


# On a Timeline

@docs onTimeline, onTimelineWith


# Rendering

@docs div, node

@docs Css, css, toCss

-}

import Animator.Timeline exposing (Timeline)
import Animator.Transition
import Color
import Html exposing (Html)
import Html.Attributes as Attr
import InternalAnim.Css as Css
import InternalAnim.Css.Props
import InternalAnim.Duration as Duration
import InternalAnim.Move as Move
import InternalAnim.Quantity as Quantity
import InternalAnim.Time as Time
import InternalAnim.Timeline as Timeline
import Time


{-| -}
type alias Attribute =
    Css.Prop


{-| -}
opacity : Float -> Attribute
opacity o =
    Css.Prop
        InternalAnim.Css.Props.ids.opacity
        "opacity"
        (Move.to o)
        InternalAnim.Css.Props.float


{-| -}
scale : Float -> Attribute
scale s =
    Css.Prop
        InternalAnim.Css.Props.ids.scale
        "scale"
        (Move.to s)
        InternalAnim.Css.Props.float


{-| -}
scaleX : Float -> Attribute
scaleX s =
    Css.Prop
        InternalAnim.Css.Props.ids.scaleX
        "scale"
        (Move.to s)
        InternalAnim.Css.Props.float


{-| -}
scaleY : Float -> Attribute
scaleY s =
    Css.Prop
        InternalAnim.Css.Props.ids.scaleY
        "scale"
        (Move.to s)
        InternalAnim.Css.Props.float


{-| -}
scaleZ : Float -> Attribute
scaleZ s =
    Css.Prop
        InternalAnim.Css.Props.ids.scaleZ
        "scale"
        (Move.to s)
        InternalAnim.Css.Props.float


{-| Given as 'turns'.
-}
rotation : Float -> Attribute
rotation n =
    Css.Prop
        InternalAnim.Css.Props.ids.rotation
        "rotate"
        (Move.to (1000 * n))
        (InternalAnim.Css.Props.turns zAxis)


{-| -}
rotationAround : { x : Float, y : Float, z : Float } -> Float -> Attribute
rotationAround axis n =
    Css.Prop
        InternalAnim.Css.Props.ids.rotation
        "rotate"
        (Move.to (1000 * n))
        (InternalAnim.Css.Props.turns axis)


zAxis : { x : Float, y : Float, z : Float }
zAxis =
    { x = 0, y = 0, z = 1 }


{-| -}
x : Float -> Attribute
x n =
    Css.Prop
        InternalAnim.Css.Props.ids.x
        "translate"
        (Move.to n)
        InternalAnim.Css.Props.float


{-| -}
y : Float -> Attribute
y n =
    Css.Prop
        InternalAnim.Css.Props.ids.y
        "translate"
        (Move.to n)
        InternalAnim.Css.Props.float


{-| -}
z : Float -> Attribute
z n =
    Css.Prop
        InternalAnim.Css.Props.ids.z
        "translate"
        (Move.to n)
        InternalAnim.Css.Props.float


{-| -}
withTransition : Animator.Transition.Transition -> Attribute -> Attribute
withTransition trans prop =
    case prop of
        Css.Prop id name move format ->
            Css.Prop id name (Move.withTransition trans move) format

        Css.ColorProp name move ->
            Css.ColorProp name (Move.withTransition trans move)


{-| -}
withStepTransition : Animator.Transition.Transition -> Step -> Step
withStepTransition trans (Step duration attrs) =
    Step duration (List.map (withTransition trans) attrs)


{-| Choosing a nice duration can depend on:

  - The size of the thing moving
  - The type of movement
  - The distance it's traveling.

So, start with a nice default and adjust it as you start to understand your specific needs.

**Note** — Here's [a very good overview on animation durations and speeds](https://uxdesign.cc/the-ultimate-guide-to-proper-use-of-animation-in-ux-10bd98614fa9).

-}
type alias Duration =
    Time.Duration


{-| -}
ms : Float -> Duration
ms =
    Duration.milliseconds


{-| -}
delay : Duration -> Animation -> Animation
delay dur (Animation { now, attrs, allowTransitions }) =
    Animation
        { allowTransitions = allowTransitions
        , now = Time.rollbackBy dur now
        , attrs = attrs
        }


{-| -}
type Animation
    = Animation
        { now : Time.Absolute
        , attrs : List Css.RenderedProp
        , allowTransitions : Move.AllowTransitions
        }


{-| -}
type Step
    = Step Duration (List Attribute)


{-| -}
set : List Attribute -> Step
set attrs =
    step Time.zeroDuration attrs


{-| -}
wait : Duration -> Step
wait dur =
    step dur []


{-| -}
step : Duration -> List Attribute -> Step
step =
    Step


{-| -}
keyframes : List Step -> Animation
keyframes steps =
    let
        imminent =
            Time.absolute (Time.millisToPosix 1)

        firstEventTime =
            imminent

        ( firstOccurring, remaining ) =
            case steps of
                [] ->
                    ( Timeline.Occurring [] imminent imminent, [] )

                (Step dur props) :: r ->
                    let
                        eventEnd =
                            Time.advanceBy dur firstEventTime
                    in
                    ( Timeline.Occurring props firstEventTime eventEnd, r )

        timeline =
            Timeline.Timeline
                { initial =
                    case steps of
                        [] ->
                            []

                        (Step _ props) :: _ ->
                            props
                , now = imminent
                , updatedAt = imminent
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
                                ( firstEventTime, [] )
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
        { allowTransitions = Move.DisallowTransitions
        , now = Timeline.getUpdatedAt timeline
        , attrs = Css.propsToRenderedProps timeline identity
        }


toOccurring : Time.Absolute -> Step -> ( Time.Absolute, Timeline.Occurring (List Attribute) )
toOccurring currentTime (Step dur props) =
    let
        time =
            Time.advanceBy dur currentTime
    in
    ( time, Timeline.Occurring props time time )


{-| -}
sequence : List Step -> Step
sequence steps =
    loopFor 1 steps


{-| -}
loop : List Step -> Step
loop steps =
    -- negative one means infinite.  I know, I know
    loopFor -1 steps


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

                Just (Css.Prop _ _ _ _) ->
                    formatColorSteps next
                        prop
                        pastSteps

                Just (Css.ColorProp _ (Move.Pos trans value _)) ->
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

                Just (Css.Prop _ _ (Move.Pos trans value _) _) ->
                    formatSteps next
                        prop
                        (Move.stepWith dur trans value :: pastSteps)

                Just (Css.ColorProp _ _) ->
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
        InternalAnim.Css.Props.noId
        name
        (Move.to n)
        InternalAnim.Css.Props.px


{-| -}
int : String -> Float -> Attribute
int name n =
    Css.Prop
        InternalAnim.Css.Props.noId
        name
        (Move.to n)
        InternalAnim.Css.Props.int


{-| -}
float : String -> Float -> Attribute
float name n =
    Css.Prop
        InternalAnim.Css.Props.noId
        name
        (Move.to n)
        InternalAnim.Css.Props.float


{-| -}
color : String -> Color.Color -> Attribute
color name colorValue =
    Css.ColorProp name
        (Move.to colorValue)


{-| -}
spinning : Duration -> Animation
spinning dur =
    keyframes
        [ loop
            [ set
                [ rotation 0
                ]
            , step dur
                [ rotation 1
                    |> withTransition
                        Animator.Transition.linear
                ]
            ]
        ]


{-| -}
pulsing : Duration -> Animation
pulsing dur =
    let
        half =
            dur
                |> Quantity.divideBy 2
    in
    keyframes
        [ loop
            [ set
                [ opacity 1
                ]
            , step half
                [ opacity 0.4
                ]
            , step half
                [ opacity 1
                ]
            ]
        ]


{-| -}
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
                    |> withTransition (Animator.Transition.bezier 0.8 0 1 1)
        in
        keyframes
            [ loop
                [ set
                    [ startingY ]
                , step half
                    [ y distance
                        |> withTransition (Animator.Transition.bezier 0 0 0.2 1)
                    ]
                , step half
                    [ startingY
                    ]
                ]
            ]


{-| -}
pinging : Duration -> Animation
pinging dur =
    keyframes
        [ loop
            [ set
                [ scale 1
                , opacity 1
                ]
            , step dur
                [ scale 1.2
                , opacity 0
                ]
            ]
        ]


{-| Animate an element on a specific timeline. Check out [`Animator.Timeline`](https://package.elm-lang.org/packages/mdgriffith/elm-animator/latest/Animator-Timeline) for more details.

This will

1.  Give you smooth transitions when an animation is interrupted.
2.  Allow you to syncronize multiple elements.

```
import Animator as Anim
import Html
import Html.Attributes


Anim.div
    (Anim.onTimeline model.timeline
        (\state ->
            if state.open then
                [ Anim.opacity 1
                ]

            else
                [ Anim.opacity 0
                , Anim.x -200
                ]
        )
    )
    [ Html.Attributes.id "my-element" ]
    [ Html.text "Hello!" ]
```

-}
onTimeline : Timeline state -> (state -> List Attribute) -> Animation
onTimeline timeline toProps =
    Animation
        { allowTransitions = Move.DisallowTransitions
        , now = Timeline.getUpdatedAt timeline
        , attrs = Css.propsToRenderedProps timeline toProps
        }


{-| -}
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
        { allowTransitions = Move.DisallowTransitions
        , now = Timeline.getUpdatedAt timeline
        , attrs = Css.propsToRenderedProps timeline toProps
        }


{-|

    import Animator as Anim
    import Html
    import Html.Attributes


    Anim.div
        (Anim.transition (Anim.ms 200)
            [ Anim.opacity <|
                if model.visible then
                    1
                else
                    0
            ]
        )
        [ Html.Attributes.id "my-element" ]
        [ Html.text "Hello!" ]

-}
transition : Duration -> List Attribute -> Animation
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
                , updatedAt = Time.absolute (Time.millisToPosix 0)
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

        renderedProps =
            Css.propsToRenderedProps timeline identity
    in
    Animation
        { now = Timeline.getUpdatedAt timeline
        , allowTransitions = Move.AllowTransitions
        , attrs = renderedProps
        }


{-| -}
toCss : Animation -> Css
toCss (Animation anim) =
    Css.toCss anim


{-| -}
div :
    Animation
    -> List (Html.Attribute msg)
    -> List (Html msg)
    -> Html msg
div (Animation anim) attrs children =
    let
        rendered =
            Css.toCss anim
    in
    Html.div
        (List.map (\( key, val ) -> Attr.style key val) rendered.props ++ attrs)
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
node name (Animation anim) attrs children =
    let
        rendered =
            Css.toCss anim
    in
    Html.node name
        (List.map (\( key, val ) -> Attr.style key val) rendered.props ++ attrs)
        (stylesheet rendered.keyframes
            :: children
        )


{-| -}
type alias Css =
    { hash : String
    , keyframes : String
    , transition : String
    , props : List ( String, String )
    }


{-| -}
css : Timeline state -> (state -> ( List Attribute, List Step )) -> Css
css timeline toPropsAndSteps =
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
    Css.toCss
        { allowTransitions = Move.DisallowTransitions
        , now = Timeline.getUpdatedAt timeline
        , attrs = Css.propsToRenderedProps timeline toProps
        }


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
