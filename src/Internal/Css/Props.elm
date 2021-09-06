module Internal.Css.Props exposing
    ( Id, ids, hash, default, defaultPosition
    , isTransformId
    , Format, format, float, int, px
    , roundFloat, floatToString
    , colorHash, name, noId, toStr, translateX, transparent, zero
    )

{-|

@docs Id, ids, hash, default, defaultPosition

@docs isTransformId

@docs Format, format, float, int, px

@docs roundFloat, floatToString

-}

import Bitwise
import Color
import Internal.Move as Move


roundFloat : Float -> Float
roundFloat f =
    toFloat (round (f * 100)) / 100


floatToString : Float -> String
floatToString f =
    String.fromFloat (roundFloat f)


transparent : Color.Color
transparent =
    Color.rgba 0 0 0 0


format : Format -> Float -> String
format form num =
    case form of
        AsFloat ->
            String.fromFloat (roundFloat num)

        AsInt ->
            String.fromInt (round num)

        Px ->
            String.fromInt (round num) ++ "px"

        TranslateX ->
            "translateX(" ++ String.fromInt (round num) ++ "px)"


type Format
    = AsFloat
    | AsInt
    | Px
    | TranslateX


float : Format
float =
    AsFloat


int : Format
int =
    AsInt


px : Format
px =
    Px


translateX : Format
translateX =
    TranslateX


{-| We make this huge because we want it last.

The order of the ids matters, as it's the order that they're rendered in.

Which we really only care about for transforms, we want them to be first.

-}
noId : Id
noId =
    100000


ids =
    { x = 0
    , y = 1
    , z = 2
    , rotation = 3
    , scale = 4
    , scaleX = 5
    , scaleY = 6
    , opacity = 13
    }


firstTransform : Id
firstTransform =
    ids.x


type alias Id =
    Int


isTransformId : Id -> Bool
isTransformId id =
    id < 12


hash : Id -> String
hash id =
    case id of
        0 ->
            "x-"

        1 ->
            "y-"

        2 ->
            "z-"

        3 ->
            "r-"

        4 ->
            "s-"

        5 ->
            "sx-"

        6 ->
            "sy-"

        13 ->
            -- opacity
            "o-"

        14 ->
            "bgc-"

        _ ->
            "unknown-"


name : Id -> String
name id =
    case id of
        13 ->
            "opacity"

        14 ->
            "background-color"

        _ ->
            "unknown"


toStr : Id -> (Float -> String)
toStr id =
    case id of
        0 ->
            \f ->
                "translateX(" ++ String.fromFloat f ++ "px)"

        1 ->
            \f ->
                "translateY(" ++ String.fromFloat f ++ "px)"

        2 ->
            \f ->
                "translateZ(" ++ String.fromFloat f ++ "px)"

        3 ->
            \f ->
                "rotate(" ++ String.fromFloat f ++ "rad)"

        4 ->
            \f ->
                "scale(" ++ String.fromFloat f ++ ")"

        5 ->
            \f ->
                "scaleX(" ++ String.fromFloat f ++ ")"

        6 ->
            \f ->
                "scaleY(" ++ String.fromFloat f ++ ")"

        13 ->
            -- opacity
            \f ->
                String.fromFloat f

        _ ->
            \f ->
                String.fromFloat f


defaultPosition : Id -> Float
defaultPosition id =
    case id of
        13 ->
            -- opacity
            1

        3 ->
            -- rotation
            0

        4 ->
            -- scale
            1

        5 ->
            -- scaley
            1

        6 ->
            -- scalex
            1

        _ ->
            0


default : Id -> Move.Move Float
default id =
    case id of
        13 ->
            zero

        _ ->
            zero


zero : Move.Move Float
zero =
    Move.to 0


colorHash : Color.Color -> String
colorHash color =
    let
        rgba =
            Color.toRgba color
    in
    String.fromInt (encode4 rgba.red rgba.green rgba.blue rgba.alpha)


encode4 : Float -> Float -> Float -> Float -> Int
encode4 one two three four =
    Bitwise.and top8 (round (one * 255))
        |> Bitwise.or
            (Bitwise.shiftLeftBy 8 (Bitwise.and top8 (round (two * 255))))
        |> Bitwise.or
            (Bitwise.shiftLeftBy 16 (Bitwise.and top8 (round (three * 255))))
        |> Bitwise.or
            (Bitwise.shiftLeftBy 24 (Bitwise.and top8 (round (four * 255))))


top8 : Int
top8 =
    Bitwise.shiftRightZfBy (32 - 8) ones


ones : Int
ones =
    Bitwise.complement 0
