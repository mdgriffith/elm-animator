module Internal.Css.Props exposing (Id, default, firstTransform, hash, ids, isTransformId, name, toStr)

import Internal.Interpolate as Interpolate


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

        _ ->
            "unknown-"


name : Id -> String
name id =
    case id of
        13 ->
            "opacity"

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
                "rotation(" ++ String.fromFloat f ++ "rad)"

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
                String.fromFloat (f / 100)

        _ ->
            \f ->
                String.fromFloat f


default : Id -> Interpolate.Movement
default id =
    case id of
        13 ->
            zero

        _ ->
            zero


zero : Interpolate.Movement
zero =
    Interpolate.Pos
        Interpolate.standardDefault
        0
