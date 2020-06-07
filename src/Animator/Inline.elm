module Animator.Inline exposing
    ( opacity
    , backgroundColor, textColor, borderColor
    , xy, rotate, scale, transform
    , style, linear, color
    )

{-| Render inline styles.

The best way is to play with the [**Checkbox**](https://github.com/mdgriffith/elm-animator/blob/master/examples/Checkbox.elm) example.

**Note** - At some point you should check out `Animator.Css`. It has a few benefits over inline styles.


# Inline Styles

@docs opacity

@docs backgroundColor, textColor, borderColor


## Transformations

@docs xy, rotate, scale, transform


## Custom

If you can't find an attribute here, you might need to use [`Animator.Inline.style`](#style) or [`Animator.Inline.color`](#color) to create it!

@docs style, linear, color

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
opacity : Timeline state -> (state -> Movement) -> Html.Attribute msg
opacity timeline lookup =
    style timeline "opacity" String.fromFloat lookup


{-| -}
textColor : Timeline state -> (state -> Color) -> Html.Attribute msg
textColor timeline lookup =
    color timeline "color" lookup


{-| -}
backgroundColor : Timeline state -> (state -> Color) -> Html.Attribute msg
backgroundColor timeline lookup =
    color timeline "background-color" lookup


{-| -}
borderColor : Timeline state -> (state -> Color) -> Html.Attribute msg
borderColor timeline lookup =
    color timeline "border-color" lookup


{-| Rotate to a specific angle, where increasing numbers move clockwise.

The actual number provided should be in radians.

Alternatively you could use [turns or degrees](https://package.elm-lang.org/packages/elm/core/latest/Basics#degrees) instead.

-}
rotate : Timeline state -> (state -> Movement) -> Html.Attribute msg
rotate timeline lookup =
    style timeline
        "transform"
        (\r -> "rotate(" ++ String.fromFloat r ++ "rad)")
        lookup


{-| -}
scale : Timeline state -> (state -> Movement) -> Html.Attribute msg
scale timeline lookup =
    style timeline
        "transform"
        (\s -> "scale(" ++ String.fromFloat s ++ ")")
        lookup


{-| -}
xy : Timeline state -> (state -> { x : Movement, y : Movement }) -> Html.Attribute msg
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
color : Timeline state -> String -> (state -> Color) -> Html.Attribute msg
color timeline name lookup =
    Attr.style name
        (Color.toCssString (Animator.color timeline lookup))


{-| -}
style : Timeline state -> String -> (Float -> String) -> (state -> Movement) -> Html.Attribute msg
style timeline name toString lookup =
    Attr.style name
        (toString (Animator.move timeline lookup))


{-| Sets the default to `linear`. See the note on [`Animator.linear`](https://package.elm-lang.org/packages/mdgriffith/elm-animator/latest/Animator#linear) for more details!
-}
linear : Timeline state -> String -> (Float -> String) -> (state -> Movement) -> Html.Attribute msg
linear timeline name toString lookup =
    Attr.style name
        (toString (Animator.linear timeline lookup))
