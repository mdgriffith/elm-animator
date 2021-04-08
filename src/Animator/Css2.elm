module Animator.Css2 exposing
    ( opacity
    , rotation, x, y, scale, scaleX, scaleY
    , color, px, int, float
    , withWobble, withImpulse, withCurve, withDelay
    , div, node
    , Css, css
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

-}

import Animator exposing (Movement, Timeline)
import Color
import Html exposing (Html)
import Html.Attributes as Attr
import Internal.Bezier as Bezier
import Internal.Css as Css
import Internal.Css.Props
import Internal.Interpolate as Interpolate


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
        (Interpolate.Pos Interpolate.standardDefault o Nothing)
        Internal.Css.Props.float


{-| -}
scale : Float -> Property
scale s =
    Css.Prop
        Internal.Css.Props.ids.scale
        ""
        (Interpolate.Pos Interpolate.standardDefault s Nothing)
        Internal.Css.Props.float


{-| -}
scaleX : Float -> Property
scaleX s =
    Css.Prop
        Internal.Css.Props.ids.scaleX
        ""
        (Interpolate.Pos Interpolate.standardDefault s Nothing)
        Internal.Css.Props.float


{-| -}
scaleY : Float -> Property
scaleY s =
    Css.Prop
        Internal.Css.Props.ids.scaleY
        ""
        (Interpolate.Pos Interpolate.standardDefault s Nothing)
        Internal.Css.Props.float


{-| -}
rotation : Float -> Property
rotation n =
    Css.Prop
        Internal.Css.Props.ids.rotation
        ""
        (Interpolate.Pos Interpolate.standardDefault n Nothing)
        Internal.Css.Props.float


{-| -}
x : Float -> Property
x n =
    Css.Prop
        Internal.Css.Props.ids.x
        ""
        (Interpolate.Pos Interpolate.standardDefault n Nothing)
        Internal.Css.Props.float


{-| -}
y : Float -> Property
y n =
    Css.Prop
        Internal.Css.Props.ids.y
        ""
        (Interpolate.Pos Interpolate.standardDefault n Nothing)
        Internal.Css.Props.float


{-| -}
withWobble : Float -> Property -> Property
withWobble wob prop =
    prop
        |> Css.applyToMovement
            (Interpolate.mapPersonality
                (\personality ->
                    { personality
                        | wobbliness =
                            clamp 0 1 wob
                    }
                )
            )


{-| -}
withImpulse : Float -> Property -> Property
withImpulse impulse prop =
    prop
        |> Css.applyToMovement
            (Interpolate.mapPersonality
                (\personality ->
                    { personality
                        | impulse =
                            impulse
                    }
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
        (Interpolate.Pos Interpolate.standardDefault n Nothing)
        Internal.Css.Props.px


{-| -}
int : String -> Float -> Property
int name n =
    Css.Prop
        Internal.Css.Props.noId
        name
        (Interpolate.Pos Interpolate.standardDefault n Nothing)
        Internal.Css.Props.int


{-| -}
float : String -> Float -> Property
float name n =
    Css.Prop
        Internal.Css.Props.noId
        name
        (Interpolate.Pos Interpolate.standardDefault n Nothing)
        Internal.Css.Props.float


{-| -}
color : String -> Color.Color -> Property
color name colorValue =
    Css.ColorProp
        { name = name
        , color = colorValue
        }


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
    in
    Html.node name
        (Attr.style "animation" rendered.animation
            :: attrs
        )
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
