module InternalAnim.Css.Props exposing
    ( Id, ids, noId, defaultPosition, groups
    , isTranslateId, isScaleId, vectorToString
    , Format, format, float, int, px, turns
    )

{-| Property identifiers and CSS value formatting for the animation renderer.

@docs Id, ids, noId, defaultPosition, groups
@docs isTranslateId, isScaleId, vectorToString
@docs Format, format, float, int, px, turns

-}


vectorToString : Id -> { x : Float, y : Float, z : Float } -> String
vectorToString id vec =
    case id of
        10 ->
            translateToString vec

        20 ->
            scaleToString vec

        _ ->
            vectorToCssString vec


translateToString : { x : Float, y : Float, z : Float } -> String
translateToString { x, y, z } =
    floatToString x ++ "px " ++ floatToString y ++ "px " ++ floatToString z ++ "px"


scaleToString : { x : Float, y : Float, z : Float } -> String
scaleToString vec =
    vectorToCssString vec


vectorToCssString : { x : Float, y : Float, z : Float } -> String
vectorToCssString { x, y, z } =
    floatToString x ++ " " ++ floatToString y ++ " " ++ floatToString z


type alias Vector =
    { x : Float
    , y : Float
    , z : Float
    }


roundFloat : Float -> Float
roundFloat f =
    toFloat (round (f * 100)) / 100


floatToString : Float -> String
floatToString f =
    String.fromFloat (roundFloat f)


format : Format -> Float -> String
format form num =
    case form of
        AsFloat ->
            String.fromFloat (roundFloat num)

        AsInt ->
            String.fromInt (round num)

        Px ->
            String.fromInt (round num) ++ "px"

        Turns vec ->
            -- Number here is 1/1000 of a turn
            vectorToCssString vec ++ " " ++ String.fromFloat (num / 1000) ++ "turn"


type Format
    = AsFloat
    | AsInt
    | Px
    | Turns Vector


turns : Vector -> Format
turns vec =
    Turns vec


float : Format
float =
    AsFloat


int : Format
int =
    AsInt


px : Format
px =
    Px


{-| Identifier for arbitrary, user-named CSS properties.
-}
noId : Id
noId =
    100000


groups : { scaling : Id, translation : Id }
groups =
    { scaling = 20
    , translation = 10
    }


ids :
    { x : Id
    , y : Id
    , z : Id
    , rotation : Id
    , scale : Id
    , scaleX : Id
    , scaleY : Id
    , scaleZ : Id
    , opacity : Id
    }
ids =
    { x = 0
    , y = 1
    , z = 2
    , rotation = 3
    , scale = 4
    , scaleX = 5
    , scaleY = 6
    , scaleZ = 7
    , opacity = 13
    }


type alias Id =
    Int


isTranslateId : Id -> Bool
isTranslateId id =
    id < 3


isScaleId : Id -> Bool
isScaleId id =
    id == 4 || id == 5 || id == 6 || id == 7


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
            -- scaleX
            1

        6 ->
            -- scaleY
            1

        7 ->
            -- scaleZ
            1

        _ ->
            0
