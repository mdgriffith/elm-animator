module Animator.Inline exposing
    ( opacity
    , backgroundColor, textColor, borderColor
    , translate, rotate, scale, transform
    )

{-|


# Inline CSS

@docs opacity

@docs backgroundColor, textColor, borderColor


## Transformations

@docs translate, rotate, scale, transform


# CSS Animations

-}

import Animator exposing (..)
import Color exposing (Color)
import Html
import Html.Attributes as Attr


{-| -}
opacity : Timeline event -> (event -> Float) -> Html.Attribute msg
opacity timeline lookup =
    Attr.style "opacity"
        (String.fromFloat (Animator.linear timeline lookup))


{-| -}
textColor : Timeline event -> (event -> Color) -> Html.Attribute msg
textColor timeline lookup =
    Attr.style "color"
        (Color.toCssString (Animator.color timeline lookup))


{-| -}
backgroundColor : Timeline event -> (event -> Color) -> Html.Attribute msg
backgroundColor timeline lookup =
    Attr.style "background-color"
        (Color.toCssString (Animator.color timeline lookup))


{-| -}
borderColor : Timeline event -> (event -> Color) -> Html.Attribute msg
borderColor timeline lookup =
    Attr.style "border-color"
        (Color.toCssString (Animator.color timeline lookup))


{-| -}
rotate : Timeline event -> (event -> Float) -> Html.Attribute msg
rotate timeline lookup =
    Attr.style "transform"
        ("rotate(" ++ String.fromFloat (Animator.float timeline lookup) ++ "rad)")


{-| -}
scale : Timeline event -> (event -> Float) -> Html.Attribute msg
scale timeline lookup =
    Attr.style "transform"
        ("scale(" ++ String.fromFloat (Animator.float timeline lookup) ++ ")")


{-| -}
translate : Timeline event -> (event -> { x : Movement, y : Movement }) -> Html.Attribute msg
translate timeline lookup =
    let
        pos =
            Animator.xy timeline lookup
    in
    Attr.style "transform"
        ("translate(" ++ String.fromFloat pos.x ++ "px, " ++ String.fromFloat pos.y ++ "px)")


{-| -}
transform :
    { scale : Float
    , rotate : Float
    , position : { x : Float, y : Float }
    }
    -> Html.Attribute msg
transform transmogrify =
    Attr.style "transform"
        (("rotate(" ++ String.fromFloat transmogrify.rotate ++ "rad)")
            ++ ("translate(" ++ String.fromFloat transmogrify.position.x ++ "px, " ++ String.fromFloat transmogrify.position.y ++ "px)")
            ++ ("scale(" ++ String.fromFloat transmogrify.scale ++ ")")
        )



{- Generating CSS Animations -}
