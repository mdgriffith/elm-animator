module Internal.Transition exposing
    ( Transition
    , atT
    , before
    , compoundKeyframes
    , firstDerivative
    , hash
    , keyframes
    , splines
    , split
    , standard
    )

{-| Current bezier formats for elm-animator

       Standard:
           x -> Time.Absolute in milliseconds
           y -> actual value being animated

        Alternatively we could store beziers in a normalized form

        Normalized:
            x -> 0-256
            y -> 0-256

        And then scale it to the duration and value domain we're expecting.

        We can also assume that this is an easing that starts at 0,0 and ends at 256,256
            (or maybe there are multiple control points in-between.

        For transitions:
            If we are passing through a state,
            we want to be able to set the velocity for the before and after curve

What do we need from a transition?

  - A list of splines in either normalized or standard form.
    -> for debugging purposes/to render as an svg.

  - As css @keyframes
    -> just render the keyframes directly

  - To a string hash
    -> for rendeirng css stuff

Goals:

    1. Can we make it where this data is adjusted as little as possible.
    2. Can we pack the splines as tightly as possible
        Do they have to be floats?  Can they be Ints?  Can they by bit-encoded ints?
    3. Conversion to css @keyframes should be as fast as possible.

-}

import Internal.Bezier as Bezier
import Internal.Bits as Bits
import Pixels


type Transition
    = Transition (List Bezier.Spline)


{-| Ideally we'd store a bezier

Standard flutter transition:

    <https://material.io/design/motion/speed.html#easing>
    cubic-bezier(0.4, 0.0, 0.2, 1);

Then, when calling `atT`, the values we get back are normalized to 0-1

We can then multiply those by our domain

-}
standard : Transition
standard =
    Transition
        [ Bezier.Spline
            { x = 0
            , y = 0
            }
            { x = 0.4
            , y = 0
            }
            { x = 0.2
            , y = 1
            }
            { x = 1
            , y = 1
            }
        ]


{-| -}
atT :
    Float
    -> Transition
    -> { position : Float, velocity : Float }
atT t (Transition curves) =
    { position = 0
    , velocity = 0
    }


firstDerivative : Float -> Transition -> Bezier.Point
firstDerivative t (Transition curves) =
    case curves of
        [] ->
            { x = 1
            , y = 1
            }

        first :: _ ->
            Bezier.firstDerivative first t


type alias Domain =
    { start : Bezier.Point
    , end : Bezier.Point
    }


{-| -}
splines : Domain -> Float -> Float -> Transition -> List Bezier.Spline
splines domain introVelocity exitVelocity (Transition lines) =
    List.map (toDomain domain introVelocity exitVelocity) lines


third : Float
third =
    1 / 3


negativeThird : Float
negativeThird =
    -1 / 3


toDomain : Domain -> Float -> Float -> Bezier.Spline -> Bezier.Spline
toDomain domain introVelocity exitVelocity (Bezier.Spline one two three four) =
    let
        totalX =
            domain.end.x - domain.start.x

        totalY =
            domain.end.y - domain.start.y

        ctrl1 =
            let
                angle =
                    atan2 introVelocity 1
            in
            { x =
                (totalX * two.x) + domain.start.x
            , y =
                (totalY * two.y) + domain.start.y
            }
                |> rotateAround angle domain.start

        ctrl2 =
            let
                angle =
                    atan2 exitVelocity 1
            in
            { x =
                (totalX * three.x) + domain.start.x
            , y =
                (totalY * three.y) + domain.start.y
            }
                |> rotateAround angle domain.end
    in
    Bezier.Spline
        domain.start
        ctrl1
        ctrl2
        domain.end


{-| -}
rotateAround : Float -> Bezier.Point -> Bezier.Point -> Bezier.Point
rotateAround radians center point =
    { x = cos radians * (point.x - center.x) - sin radians * (point.y - center.y) + center.x
    , y = sin radians * (point.x - center.x) + cos radians * (point.y - center.y) + center.y
    }


translateBy : Bezier.Point -> Bezier.Point -> Bezier.Point
translateBy one two =
    { x = one.x + two.x
    , y = one.y + two.y
    }


scaleBy : Float -> Bezier.Point -> Bezier.Point
scaleBy k v =
    { x = k * v.x
    , y = k * v.y
    }


scaleAbout : Bezier.Point -> Float -> Bezier.Point -> Bezier.Point
scaleAbout p0 k p =
    { x = p0.x + k * (p.x - p0.x)
    , y = p0.y + k * (p.y - p0.y)
    }


{-| We are vector v and want the component in the direction d.
-}
componentIn : Bezier.Point -> Bezier.Point -> Float
componentIn d v =
    v.x * d.x + v.y * d.y


zeroPoint : Bezier.Point
zeroPoint =
    { x = 0
    , y = 0
    }


scaleTo : Float -> Bezier.Point -> Bezier.Point
scaleTo q v =
    let
        largestComponent =
            max (abs v.x) (abs v.y)
    in
    if largestComponent == 0 then
        zeroPoint

    else
        let
            scaledX =
                v.x / largestComponent

            scaledY =
                v.y / largestComponent

            scaledLength =
                sqrt (scaledX * scaledX + scaledY * scaledY)
        in
        { x = q * scaledX / scaledLength
        , y = q * scaledY / scaledLength
        }



-- start startVelocity end endVelocity


{-| -}
hash : Float -> Float -> Float -> Float -> Float -> Transition -> String
hash start startVelocity end endVelocity t (Transition curves) =
    ""


{-| -}
keyframes : (Float -> String) -> Transition -> String
keyframes toString (Transition curves) =
    ""


{-|

    *Note* need to guarantee that the strings passed to toStr
    are given in the same order as defined in `transitions`

-}
compoundKeyframes : (List String -> String) -> List ( Float -> String, Transition ) -> String
compoundKeyframes toStr transitions =
    ""


{-| -}
conflicting : Transition -> Transition -> Bool
conflicting (Transition one) (Transition two) =
    False


{-| -}
before : Float -> Transition -> Transition
before t (Transition lines) =
    Transition lines


{-| -}
split : Float -> Transition -> { before : Transition, after : Transition }
split t (Transition lines) =
    { before = Transition lines
    , after = Transition lines
    }


{-| Modifies the first Bezier curve so that the first oint and first control point have the correct
angle between them.
-}
withIntroVelocity : Float -> Transition -> Transition
withIntroVelocity vel (Transition t) =
    Transition t


{-| -}
withExitVelocity : Float -> Transition -> Transition
withExitVelocity vel (Transition t) =
    Transition t
