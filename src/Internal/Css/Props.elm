module Internal.Css.Props exposing (Id, default, firstTransform, ids, isTransformId, name, toStr)

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



zero =
    Interpolate.Pos 
                Interpolate.standardDefault
                0