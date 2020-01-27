module Animator.CSS exposing
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
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Keyed


type Attribute
    = Attribute


type Animated msg
    = Animated String (List Attribute) (List (Html.Attribute msg)) (List (Html msg))


div : List (Html.Attribute msg) -> List ( String, Animated msg ) -> Html msg
div attrs children =
    let
        ( htmlChildren, styles ) =
            List.foldl gatherChildren ( [], "" ) children
    in
    Html.Keyed.node "div"
        attrs
        (( "animator-stylesheet", stylesheet styles ) :: List.reverse htmlChildren)


gatherChildren ( key, animatedNode ) ( existingChildren, existingStyles ) =
    let
        ( newStyles, newNode ) =
            render animatedNode
    in
    ( ( key, newNode ) :: existingChildren
    , newStyles ++ existingStyles
    )


render (Animated name animAttrs htmlAttrs children) =
    let
        ( class, keyframes ) =
            List.foldl renderAttrs ( "", "" ) animAttrs
    in
    ( keyframes
    , Html.node name (Attr.class class :: htmlAttrs) children
    )


renderAttrs attr ( class, keyframes ) =
    ( class, keyframes )



{- CSS animation generation

   animation-timing-function: linear;
   animation-duration: 6s;
   animation-fill-mode: forwards;


   animation-iteration-count: 3;



   -- https://codepen.io/Guilh/pen/yldGp/
   -- Sprites
   -- https://developer.mozilla.org/en-US/docs/Web/CSS/animation-timing-function
   --     -> animation-timing-function: steps(n, jump-both or whatever)
   --     -> keyframes are literal sprite positions



-}


renderAttribute :
    (Timeline state -> (state -> anchor) -> value)
    -> Timeline state
    -> (state -> anchor)
    -> (value -> ( String, String ))
    ->
        { framesPerSecond : Float
        }
    -> ( String, String )
renderAttribute myTimeline toPos renderer config =
    -- let
    --     startTimeInMs =
    --         Time.posixToMillis config.start
    --     durationInMs =
    --         Time.posixToMillis config.end
    --             - startTimeInMs
    --     frameCount =
    --         (toFloat durationInMs / 1000) * config.framesPerSecond
    --     frameSize =
    --         1000 / config.framesPerSecond
    --     frames =
    --         List.range 0 (ceiling frameCount)
    -- in
    -- List.foldl
    --     (\i rendered ->
    --         let
    --             currentTime =
    --                 Time.millisToPosix (round (toFloat startTimeInMs + (toFloat i * frameSize)))
    --         in
    --         { time = currentTime
    --         , value = Animator.details (Internal.Timeline.atTime currentTime myTimeline) toPos
    --         }
    --             :: rendered
    --     )
    --     []
    --     frames
    Debug.todo ""


stylesheet : String -> Html msg
stylesheet str =
    Html.node "style"
        []
        [ Html.text str
        ]


animated : List Attribute -> List (Html.Attribute msg) -> List (Html msg) -> Animated msg
animated =
    Animated "div"


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
