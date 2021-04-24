module Animator.Css2 exposing
    ( opacity
    , rotation, x, y, scale, scaleX, scaleY
    , color, px, int, float
    , withWobble, withImpulse, withCurve, withDelay
    , div, node
    , Css, css
    , watching
    , Attribute
    )

{-|

@docs Property

@docs opacity

@docs rotation, x, y, scale, scaleX, scaleY

@docs color, px, int, float

@docs withWobble, withImpulse, withCurve, withDelay


# Rendering

@docs div, node

@docs Css, css

@docs watching

-}

import Animator exposing (Animator, Movement, Timeline)
import Color
import Html exposing (Html)
import Html.Attributes as Attr
import Internal.Bezier as Bezier
import Internal.Css as Css
import Internal.Css.Props
import Internal.Interpolate as Interpolate
import Internal.Timeline as Timeline
import Internal.Transition as Transition


type Attribute
    = Attr Css.Prop
    | Batch (List Css.Prop)


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

-}
opacity : Float -> Property
opacity o =
    Css.Prop
        Internal.Css.Props.ids.opacity
        "opacity"
        (Interpolate.Pos Transition.standard o Nothing)
        Internal.Css.Props.float


{-| -}
scale : Float -> Property
scale s =
    Css.Prop
        Internal.Css.Props.ids.scale
        ""
        (Interpolate.Pos Transition.standard s Nothing)
        Internal.Css.Props.float


{-| -}
scaleX : Float -> Property
scaleX s =
    Css.Prop
        Internal.Css.Props.ids.scaleX
        ""
        (Interpolate.Pos Transition.standard s Nothing)
        Internal.Css.Props.float


{-| -}
scaleY : Float -> Property
scaleY s =
    Css.Prop
        Internal.Css.Props.ids.scaleY
        ""
        (Interpolate.Pos Transition.standard s Nothing)
        Internal.Css.Props.float


{-| -}
rotation : Float -> Property
rotation n =
    Css.Prop
        Internal.Css.Props.ids.rotation
        ""
        (Interpolate.Pos Transition.standard n Nothing)
        Internal.Css.Props.float


{-| -}
x : Float -> Property
x n =
    Css.Prop
        Internal.Css.Props.ids.x
        ""
        (Interpolate.Pos Transition.standard n Nothing)
        Internal.Css.Props.float


{-| -}
y : Float -> Property
y n =
    Css.Prop
        Internal.Css.Props.ids.y
        ""
        (Interpolate.Pos Transition.standard n Nothing)
        Internal.Css.Props.float


{-| -}
withWobble : Float -> Property -> Property
withWobble wob prop =
    prop
        |> Css.applyToMovement
            (Interpolate.mapTransition
                (\personality ->
                    Transition.wobble wob
                )
            )


{-| -}
withImpulse : Float -> Property -> Property
withImpulse impulse prop =
    prop
        |> Css.applyToMovement
            (Interpolate.mapTransition
                (\personality ->
                    Debug.todo "where to store impulse?"
                )
            )


{-| -}
withDelay : Animator.Duration -> Property -> Property
withDelay dur p =
    p


{-| -}
withCurve : Bezier.Spline -> Property -> Property
withCurve spline p =
    p


type Step
    = Step Animator.Duration (List Attribute)


{-| -}
sequence : List Step -> Attribute
sequence steps =
    Debug.todo ""


{-| -}
loop : List Step -> Attribute
loop steps =
    -- Batch <|
    -- List.map
    --     (\(Step dur attrs) ->
    --         Batch []
    --     )
    --     steps
    Debug.todo ""


{-| -}
repeat : Int -> List Step -> Attribute
repeat n steps =
    Debug.todo ""


{-| -}
px : String -> Float -> Property
px name n =
    Css.Prop
        Internal.Css.Props.noId
        name
        (Interpolate.Pos Transition.standard n Nothing)
        Internal.Css.Props.px


{-| -}
int : String -> Float -> Property
int name n =
    Css.Prop
        Internal.Css.Props.noId
        name
        (Interpolate.Pos Transition.standard n Nothing)
        Internal.Css.Props.int


{-| -}
float : String -> Float -> Property
float name n =
    Css.Prop
        Internal.Css.Props.noId
        name
        (Interpolate.Pos Transition.standard n Nothing)
        Internal.Css.Props.float


{-| -}
color : String -> Color.Color -> Property
color name colorValue =
    Css.ColorProp name
        (Interpolate.Pos Transition.standard 1 Nothing)
        colorValue


{--}
div :
    Timeline state
    -> (state -> List Property)
    -> List (Html.Attribute msg)
    -> List (Html msg)
    -> Html msg
div =
    node "div"


{-| -}
node :
    String
    -> Timeline state
    -> (state -> List Property)
    -> List (Html.Attribute msg)
    -> List (Html msg)
    -> Html msg
node name timeline toProps attrs children =
    let
        rendered =
            css timeline toProps

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
watching get set (Timeline.Animator isRunning updateModel) =
    Timeline.Animator
        (\model ->
            if isRunning model then
                True

            else
                let
                    tl =
                        get model
                in
                Timeline.hasChanged tl || Timeline.justInitialized tl
        )
        (\now model ->
            let
                newModel =
                    updateModel now model
            in
            set (Timeline.update now (get newModel)) newModel
        )
