module Animator.Inline exposing
    ( opacity
    , backgroundColor, textColor, borderColor
    , style, color
    , xy, rotate, scale, transform
    )

{-| This module contains some functions to render inline styles.

The best way to get familiar with how to use this is to play with the [**Checkbox**](https://github.com/mdgriffith/elm-animator/blob/master/examples/Checkbox.elm) example.

**Note** - You'll need to make a decision between this module and `Animator.Css`.
My general thinking is that `Inline` is more immediately intuitve, but `Css` has better performance, at least on Chrome.


# Inline CSS

@docs opacity

@docs backgroundColor, textColor, borderColor


## Custom

If you can't find an attribute here, you might need to use `Animator.Inline.style` or `Animator.Inline.color` to create it!

@docs style, color


## Transformations

@docs xy, rotate, scale, transform

-}

import Animator exposing (..)
import Color exposing (Color)
import Html
import Html.Attributes as Attr


{-| Change the opacity for an element.

Here's what this looks like in practice.

    div
        [ Animator.Inline.opacity model.checked <|
            \state ->
                if state then
                    Animator.at 1

                else
                    Animator.at 0
        ]
        [ Html.text "Hello!" ]

-}
opacity : Timeline event -> (event -> Movement) -> Html.Attribute msg
opacity timeline lookup =
    style timeline "opacity" String.fromFloat lookup


{-| -}
textColor : Timeline event -> (event -> Color) -> Html.Attribute msg
textColor timeline lookup =
    color timeline "color" lookup


{-| -}
backgroundColor : Timeline event -> (event -> Color) -> Html.Attribute msg
backgroundColor timeline lookup =
    color timeline "background-color" lookup


{-| -}
borderColor : Timeline event -> (event -> Color) -> Html.Attribute msg
borderColor timeline lookup =
    color timeline "border-color" lookup


{-| -}
rotate : Timeline event -> (event -> Movement) -> Html.Attribute msg
rotate timeline lookup =
    style timeline
        "transform"
        (\r -> "rotate(" ++ String.fromFloat r ++ "rad)")
        lookup


{-| -}
scale : Timeline event -> (event -> Movement) -> Html.Attribute msg
scale timeline lookup =
    style timeline
        "transform"
        (\s -> "scale(" ++ String.fromFloat s ++ ")")
        lookup


{-| -}
xy : Timeline event -> (event -> { x : Movement, y : Movement }) -> Html.Attribute msg
xy timeline lookup =
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


{-| -}
color : Timeline event -> String -> (event -> Color) -> Html.Attribute msg
color timeline name lookup =
    Attr.style name
        (Color.toCssString (Animator.color timeline lookup))


{-| -}
style : Timeline event -> String -> (Float -> String) -> (event -> Movement) -> Html.Attribute msg
style timeline name toString lookup =
    Attr.style name
        (toString (Animator.move timeline lookup))
