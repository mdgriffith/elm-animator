module InternalAnim.Css.Props exposing
    ( Id, ids, hash, default, defaultPosition, groups
    , isTranslateId, isScaleId, initVector, initVectorState, updateVectorById
    , Format, format, float, int, px, turns
    , isGroup
    , VectorSlot(..), colorHash, groupToCompoundId, noId, transparent, vectorSlotToId, vectorToString
    )

{-|

@docs Id, ids, hash, default, defaultPosition, groups

@docs isTranslateId, isScaleId, initVector, initVectorState, updateVectorById

@docs Format, format, float, int, px, turns

@docs translateToString, isGroup

-}

import Bitwise
import Color
import InternalAnim.Hash as Hash
import InternalAnim.Move as Move


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


type VectorSlot
    = X
    | Y
    | Z


type alias Vector =
    { x : Float
    , y : Float
    , z : Float
    }


type alias VectorState =
    { x : Move.State
    , y : Move.State
    , z : Move.State
    }


updateVectorById : Id -> val -> { x : val, y : val, z : val } -> { x : val, y : val, z : val }
updateVectorById id val vec =
    case id of
        0 ->
            { vec | x = val }

        1 ->
            { vec | y = val }

        2 ->
            { vec | z = val }

        4 ->
            { x = val, y = val, z = val }

        5 ->
            { vec | x = val }

        6 ->
            { vec | y = val }

        7 ->
            { vec | z = val }

        _ ->
            vec


initVectorState : Id -> Move.State -> VectorState
initVectorState id state =
    case id of
        0 ->
            { x = state, y = Move.toState 0, z = Move.toState 0 }

        1 ->
            { x = Move.toState 0, y = state, z = Move.toState 0 }

        2 ->
            { x = Move.toState 0, y = Move.toState 0, z = state }

        4 ->
            -- Scale all
            { x = state, y = state, z = state }

        5 ->
            -- scale x
            { x = state, y = Move.toState 1, z = Move.toState 1 }

        6 ->
            -- scale y
            { x = Move.toState 1, y = state, z = Move.toState 1 }

        7 ->
            -- scale z
            { x = Move.toState 1, y = Move.toState 1, z = state }

        _ ->
            { x = Move.toState 0, y = Move.toState 0, z = Move.toState 0 }


initVector : Id -> Float -> Vector
initVector id val =
    case id of
        0 ->
            { x = val, y = 0, z = 0 }

        1 ->
            { x = 0, y = val, z = 0 }

        2 ->
            { x = 0, y = 0, z = val }

        4 ->
            -- Scale all
            { x = val, y = val, z = val }

        5 ->
            -- scale x
            { x = val, y = 1, z = 1 }

        6 ->
            -- scale y
            { x = 1, y = val, z = 1 }

        7 ->
            -- scale z
            { x = 1, y = 1, z = val }

        _ ->
            { x = 0, y = 0, z = 0 }


{-| Giving the scaling group, return the property that sets all scaling/
-}
groupToCompoundId : Id -> Maybe Id
groupToCompoundId groupId =
    case groupId of
        10 ->
            Nothing

        20 ->
            Just 4

        _ ->
            Nothing


vectorSlotToId : Id -> VectorSlot -> Id
vectorSlotToId id slot =
    case id of
        10 ->
            case slot of
                X ->
                    0

                Y ->
                    1

                Z ->
                    2

        20 ->
            case slot of
                X ->
                    5

                Y ->
                    6

                Z ->
                    7

        _ ->
            0


roundFloat : Float -> Float
roundFloat f =
    toFloat (round (f * 100)) / 100


floatToString : Float -> String
floatToString f =
    String.fromFloat (roundFloat f)


transparent : Color.Color
transparent =
    Color.rgba 0 0 0 0


hashFormat : Format -> Float -> String
hashFormat form num =
    case form of
        AsFloat ->
            Hash.float num

        AsInt ->
            String.fromInt (round num)

        Px ->
            String.fromInt (round num) ++ "px"

        Turns _ ->
            String.fromInt (round num)


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


{-| We make this huge because we want it last.

The order of the ids matters, as it's the order that they're rendered in.

Which we really only care about for transforms, we want them to be first.

-}
noId : Id
noId =
    100000


isGroup : Id -> Id -> Bool
isGroup groupId id =
    case groupId of
        10 ->
            case id of
                0 ->
                    True

                1 ->
                    True

                3 ->
                    True

                _ ->
                    False

        20 ->
            case id of
                4 ->
                    True

                5 ->
                    True

                6 ->
                    True

                7 ->
                    True

                _ ->
                    False

        _ ->
            False


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


hash :
    { props
        | id : Id
        , name : String
        , format : Format
    }
    -> Float
    -> String
hash p val =
    if known p.id then
        hashId p.id ++ hashFormat p.format val

    else
        p.name ++ hashFormat p.format val


known : Id -> Bool
known id =
    case id of
        0 ->
            True

        1 ->
            True

        2 ->
            True

        3 ->
            True

        4 ->
            True

        5 ->
            True

        6 ->
            True

        13 ->
            True

        14 ->
            True

        _ ->
            False


hashId : Id -> String
hashId id =
    case id of
        0 ->
            "x"

        1 ->
            "y"

        2 ->
            "z"

        3 ->
            "r"

        4 ->
            "s"

        5 ->
            "sx"

        6 ->
            "sy"

        13 ->
            -- opacity
            "o"

        14 ->
            "bgc"

        _ ->
            "unknown"


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

        7 ->
            -- scalez
            1

        _ ->
            0


default : Id -> Move.Move Float
default id =
    case id of
        13 ->
            Move.to 1

        4 ->
            Move.to 1

        5 ->
            Move.to 1

        6 ->
            Move.to 1

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
