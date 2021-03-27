module Animator.Css2 exposing
    ( Property
    , opacity
    , rotation, x, y, z, scale, scaleX, scaleY
    , div, node
    , Css, css
    , color, px, int, float
    )

{-|

@docs Property

@docs opacity

@docs rotation, x, y, z, scale, scaleX, scaleY

@docs div, node

@docs Css, css


# Custom

@docs color, px, int, float

-}

import Animator exposing (Movement, Timeline)
import Color
import Html exposing (Html)
import Html.Attributes as Attr
import Internal.Css as Css
import Internal.Css.Props
import Internal.Interpolate as Interpolate


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
opacity : Movement -> Property
opacity o =
    Css.Prop
        Internal.Css.Props.ids.opacity
        "opacity"
        (Interpolate.withStandardDefault o)
        Internal.Css.Props.float


{-| -}
scale : Movement -> Property
scale s =
    Css.Prop
        Internal.Css.Props.ids.scale
        ""
        (Interpolate.withStandardDefault s)
        Internal.Css.Props.float


{-| -}
scaleX : Movement -> Property
scaleX s =
    Css.Prop
        Internal.Css.Props.ids.scaleX
        ""
        (Interpolate.withStandardDefault s)
        Internal.Css.Props.float


{-| -}
scaleY : Movement -> Property
scaleY s =
    Css.Prop
        Internal.Css.Props.ids.scaleY
        ""
        (Interpolate.withStandardDefault s)
        Internal.Css.Props.float


{-| -}
rotation : Movement -> Property
rotation n =
    Css.Prop
        Internal.Css.Props.ids.rotation
        ""
        (Interpolate.withStandardDefault n)
        Internal.Css.Props.float


{-| -}
x : Movement -> Property
x n =
    Css.Prop
        Internal.Css.Props.ids.x
        ""
        (Interpolate.withStandardDefault n)
        Internal.Css.Props.float


{-| -}
y : Movement -> Property
y n =
    Css.Prop
        Internal.Css.Props.ids.y
        ""
        (Interpolate.withStandardDefault n)
        Internal.Css.Props.float


{-| -}
z : Movement -> Property
z n =
    Css.Prop
        Internal.Css.Props.ids.z
        ""
        (Interpolate.withStandardDefault n)
        Internal.Css.Props.float


{-| -}
px : String -> Movement -> Property
px name n =
    Css.Prop
        Internal.Css.Props.noId
        name
        (Interpolate.withStandardDefault n)
        Internal.Css.Props.px


{-| -}
int : String -> Movement -> Property
int name n =
    Css.Prop
        Internal.Css.Props.noId
        name
        (Interpolate.withStandardDefault n)
        Internal.Css.Props.int


{-| -}
float : String -> Movement -> Property
float name n =
    Css.Prop
        Internal.Css.Props.noId
        name
        (Interpolate.withStandardDefault n)
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
