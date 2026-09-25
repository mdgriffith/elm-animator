module Animator exposing
    ( Animation, transition
    , Attribute, opacity
    , rotation, rotationAround
    , x, y, z
    , scale, scaleX, scaleY, scaleZ
    , color, px, int, float
    , withTransition, withStepTransition
    , Duration, ms
    , spinning, pulsing, bouncing, pinging
    , Step, set, wait, step
    , keyframes, loop, loopFor, sequence
    , onTimeline, onTimelineWith
    , div, node
    , Css, css, toCss
    )

{-| CSS animation, with a single renderer for transitions, sequences, and timelines.

@docs Animation, transition
@docs Attribute, opacity
@docs rotation, rotationAround
@docs x, y, z
@docs scale, scaleX, scaleY, scaleZ
@docs color, px, int, float
@docs withTransition, withStepTransition
@docs Duration, ms


# Premade

These are ordinary keyframe sequences; inspect their definitions to make your own.

@docs spinning, pulsing, bouncing, pinging


# Sequences

    import Animator as Anim
    import Html

    Anim.div
        (Anim.keyframes
            [ Anim.loop
                [ Anim.set [ Anim.opacity 1 ]
                , Anim.wait (Anim.ms 200)
                , Anim.step (Anim.ms 200) [ Anim.opacity 0 ]
                ]
            ]
        )
        []
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
import InternalAnim.Css.Props as Props
import InternalAnim.Duration as Duration
import InternalAnim.Move as Move
import InternalAnim.Property exposing (Prop(..))
import InternalAnim.Quantity as Quantity
import InternalAnim.Render as Render
import InternalAnim.Time as Time


{-| A CSS property and its target value and transition curve.
-}
type alias Attribute =
    Prop


{-| -}
opacity : Float -> Attribute
opacity value =
    Prop Props.ids.opacity "opacity" (Move.to value) Props.float


{-| Scale all three axes.
-}
scale : Float -> Attribute
scale value =
    Prop Props.ids.scale "scale" (Move.to value) Props.float


{-| -}
scaleX : Float -> Attribute
scaleX value =
    Prop Props.ids.scaleX "scale" (Move.to value) Props.float


{-| -}
scaleY : Float -> Attribute
scaleY value =
    Prop Props.ids.scaleY "scale" (Move.to value) Props.float


{-| -}
scaleZ : Float -> Attribute
scaleZ value =
    Prop Props.ids.scaleZ "scale" (Move.to value) Props.float


{-| Rotation in turns around the Z axis.
-}
rotation : Float -> Attribute
rotation =
    rotationAround { x = 0, y = 0, z = 1 }


{-| Rotation in turns around the given axis.
-}
rotationAround : { x : Float, y : Float, z : Float } -> Float -> Attribute
rotationAround axis value =
    Prop Props.ids.rotation "rotate" (Move.to (1000 * value)) (Props.turns axis)


{-| Translation in pixels.
-}
x : Float -> Attribute
x value =
    Prop Props.ids.x "translate" (Move.to value) Props.float


{-| Translation in pixels.
-}
y : Float -> Attribute
y value =
    Prop Props.ids.y "translate" (Move.to value) Props.float


{-| Translation in pixels.
-}
z : Float -> Attribute
z value =
    Prop Props.ids.z "translate" (Move.to value) Props.float


{-| Attributes use `Animator.Transition.standard` by default.
-}
withTransition : Animator.Transition.Transition -> Attribute -> Attribute
withTransition curve attribute =
    case attribute of
        Prop id name movement format ->
            Prop id name (Move.withTransition curve movement) format

        ColorProp name movement ->
            ColorProp name (Move.withTransition curve movement)


{-| Apply a curve to all properties in a step, including nested sequences.
-}
withStepTransition : Animator.Transition.Transition -> Step -> Step
withStepTransition curve animationStep =
    case animationStep of
        Render.Step duration attrs ->
            Render.Step duration (List.map (withTransition curve) attrs)

        Render.Repeat count steps ->
            Render.Repeat count (List.map (withStepTransition curve) steps)


{-| A duration. Start with a short duration and adjust for the size and distance
of the movement.
-}
type alias Duration =
    Time.Duration


{-| A duration in milliseconds.
-}
ms : Float -> Duration
ms =
    Duration.milliseconds


{-| An animation ready to render as HTML or CSS.
-}
type Animation
    = Animation Render.Css


{-| A step or nested sequence of steps.
-}
type alias Step =
    Render.Step


{-| Immediately set properties.
-}
set : List Attribute -> Step
set =
    step Time.zeroDuration


{-| Hold the current properties for a duration.
-}
wait : Duration -> Step
wait duration =
    step duration []


{-| Move to these properties, preserving properties omitted from the step.
-}
step : Duration -> List Attribute -> Step
step =
    Render.Step


{-| Render a sequence using CSS keyframe animations.
-}
keyframes : List Step -> Animation
keyframes steps =
    Animation (Render.keyframes steps)


{-| Group steps into a sequence that runs once.
-}
sequence : List Step -> Step
sequence =
    loopFor 1


{-| Repeat indefinitely.
-}
loop : List Step -> Step
loop =
    loopFor -1


{-| Repeat a sequence. Zero skips it; a negative count repeats indefinitely.
-}
loopFor : Int -> List Step -> Step
loopFor =
    Render.Repeat


{-| A property measured in pixels.
-}
px : String -> Float -> Attribute
px name value =
    Prop Props.noId name (Move.to value) Props.px


{-| A property rendered as a rounded integer.
-}
int : String -> Float -> Attribute
int name value =
    Prop Props.noId name (Move.to value) Props.int


{-| A unitless numeric property.
-}
float : String -> Float -> Attribute
float name value =
    Prop Props.noId name (Move.to value) Props.float


{-| A color property.
-}
color : String -> Color.Color -> Attribute
color name value =
    ColorProp name (Move.to value)


{-| -}
spinning : Duration -> Animation
spinning duration =
    keyframes
        [ loop
            [ set [ rotation 0 ]
            , step duration [ rotation 1 |> withTransition Animator.Transition.linear ]
            ]
        ]


{-| -}
pulsing : Duration -> Animation
pulsing duration =
    let
        half =
            Quantity.divideBy 2 duration
    in
    keyframes
        [ loop
            [ set [ opacity 1 ]
            , step half [ opacity 0.4 ]
            , step half [ opacity 1 ]
            ]
        ]


{-| The duration covers a complete out-and-back bounce. Distance is in pixels:
positive moves down, negative moves up.
-}
bouncing : Duration -> Float -> Animation
bouncing duration distance =
    if Time.isZeroDuration duration then
        keyframes []

    else
        let
            half =
                Quantity.divideBy 2 duration

            startingY =
                y 0 |> withTransition (Animator.Transition.bezier 0.8 0 1 1)
        in
        keyframes
            [ loop
                [ set [ startingY ]
                , step half [ y distance |> withTransition (Animator.Transition.bezier 0 0 0.2 1) ]
                , step half [ startingY ]
                ]
            ]


{-| -}
pinging : Duration -> Animation
pinging duration =
    keyframes
        [ loop
            [ set [ scale 1, opacity 1 ]
            , step duration [ scale 1.2, opacity 0 ]
            ]
        ]


{-| Animate on a timeline, allowing multiple elements to synchronize and handling
interruptions from their sampled positions. Uses CSS keyframe animations.

Omitted properties return to their defaults: translation and rotation to `0`,
scale and opacity to `1`. In contrast, `step` keeps omitted properties as they are.

-}
onTimeline : Timeline state -> (state -> List Attribute) -> Animation
onTimeline timeline lookup =
    onTimelineWith timeline (\state -> ( lookup state, [] ))


{-| Animate to a state's properties, then run its resting steps. The steps are
interrupted when the next timeline transition begins.
-}
onTimelineWith : Timeline state -> (state -> ( List Attribute, List Step )) -> Animation
onTimelineWith timeline lookup =
    Animation (Render.onTimeline timeline lookup)


{-| Animate a change to these properties using native CSS transitions where
possible. Springs use native CSS `linear(...)` easing, which requires a browser
with support for that timing function. Incompatible curves within a compound
property use keyframes instead.

Native springs follow CSS transition reversal rules. Use a timeline when spring
interruptions need to preserve incoming velocity.

    Animator.div
        (Animator.transition (Animator.ms 200)
            [ Animator.opacity
                (if model.visible then
                    1

                 else
                    0
                )
            ]
        )
        []
        [ Html.text "Hello!" ]

-}
transition : Duration -> List Attribute -> Animation
transition duration props =
    Animation (Render.transition duration props)


{-| Extract generated CSS for integration with a different view library.
-}
toCss : Animation -> Css
toCss (Animation rendered) =
    rendered


{-| -}
div : Animation -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
div animation attrs children =
    let
        rendered =
            toCss animation
    in
    Html.div
        (List.map (\( key, value ) -> Attr.style key value) rendered.props ++ attrs)
        (stylesheet rendered.keyframes :: children)


{-| -}
node : String -> Animation -> List (Html.Attribute msg) -> List (Html msg) -> Html msg
node name animation attrs children =
    let
        rendered =
            toCss animation
    in
    Html.node name
        (List.map (\( key, value ) -> Attr.style key value) rendered.props ++ attrs)
        (stylesheet rendered.keyframes :: children)


{-| Generated keyframes, transitions, inline properties, and an identity hash.
-}
type alias Css =
    Render.Css


{-| Equivalent to `onTimelineWith` followed by `toCss`.
-}
css : Timeline state -> (state -> ( List Attribute, List Step )) -> Css
css timeline lookup =
    onTimelineWith timeline lookup |> toCss


stylesheet : String -> Html msg
stylesheet source =
    if source == "" then
        Html.text ""

    else
        Html.node "style" [] [ Html.text source ]
