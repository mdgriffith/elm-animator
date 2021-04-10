module Internal.Transition exposing
    ( Transition
    , standard
    , initialVelocity, atX
    , split, before
    , hash, keyframes, compoundKeyframes
    , splines
    )

{-|

@docs Transition

@docs standard

@docs initialVelocity, atX

@docs split, before

@docs hash, keyframes, compoundKeyframes

@docs splines

Current bezier formats for elm-animator

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
import Internal.Spring as Spring
import Pixels
import Quantity


{-| A transition are all the bezier curves between A and B that we want to transition through.

The transition does not know it's literal start and end points, it starts and ends at (0,0) -> (1,1).

We can have a few different flavors of transition.

    1. Standard transition which has a single bezier to describe the motion (transition)
    2. A "trail" to follow, which describes a list of beziers to follow to get from A to B
    3. A "wobble", which is a dynamically calculated trail based on a spring.

There are likewise three situations a transition will be in.

    1. A -> B.  In which case, things progress normally.  We can:
        - Set an initial velocity if we want.

    2. A -> B -> C.  In this case, we're passing through B.
        - For standard transitions we modify the velocity at B so it's continuous and doesn't stop.
        - For "trails" we will similarly adjust the start and end velocity so that things are continuous
        - For spings/wobble, we will settle completely at B before continuing on.
            However in the future could we calculate a spring that "settles" only when it reaches a certain velocity?

-}
type Transition
    = Transition Bezier.Spline
    | Trail (List Bezier.Spline)
    | Wobble Float


{-| Ideally we'd store a bezier

Standard flutter transition:

    <https://material.io/design/motion/speed.html#easing>
    cubic-bezier(0.4, 0.0, 0.2, 1);

Then, when calling `atT`, the values we get back are normalized to 0-1

We can then multiply those by our domain

-}
standard : Transition
standard =
    Transition <|
        Bezier.Spline
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


{-| -}
atX :
    Float
    -> Domain
    -> Float
    -> Float
    -> Transition
    ->
        { position : Float
        , velocity : Float
        }
atX progress domain introVelocity exitVelocity transition =
    case transition of
        Transition spline ->
            spline
                |> toDomain domain introVelocity exitVelocity
                |> posVel progress

        Trail trail ->
            onTrail progress domain introVelocity exitVelocity trail

        Wobble wobble ->
            let
                totalX =
                    domain.end.x - domain.start.x

                params =
                    Spring.select wobble
                        (Quantity.Quantity totalX)
            in
            Spring.analytical params
                (Quantity.Quantity (totalX * progress))
                domain.end.y
                { position = domain.start.y
                , velocity = introVelocity
                }


posVel progress spline =
    let
        current =
            Bezier.atX progress spline

        firstDeriv =
            Bezier.firstDerivative spline current.t
    in
    { position =
        current.point.y
    , velocity =
        firstDeriv.y / firstDeriv.x
    }


onTrail :
    Float
    -> Domain
    -> Float
    -> Float
    -> List Bezier.Spline
    ->
        { position : Float
        , velocity : Float
        }
onTrail progress domain introVelocity exitVelocity trail =
    case trail of
        [] ->
            { position = domain.end.y
            , velocity = 0
            }

        spline :: remain ->
            if Bezier.firstX spline <= progress then
                spline
                    |> toDomain domain introVelocity exitVelocity
                    |> posVel progress

            else
                onTrail progress domain introVelocity exitVelocity remain


zeroVelocity : Float
zeroVelocity =
    0


initialVelocity : Transition -> Float
initialVelocity transition =
    case transition of
        Transition spline ->
            let
                firstDeriv =
                    -- at t == 0, the first derivative vector will always be 0,0
                    -- so we cheat in slightly.
                    Bezier.firstDerivative spline 0.001
            in
            if firstDeriv.x == 0 then
                zeroVelocity

            else
                firstDeriv.y / firstDeriv.x

        Trail trail ->
            case trail of
                [] ->
                    zeroVelocity

                spline :: _ ->
                    let
                        firstDeriv =
                            -- at t == 0, the first derivative vector will always be 0,0
                            -- so we cheat in slightly.
                            Bezier.firstDerivative spline 0.001
                    in
                    if firstDeriv.x == 0 then
                        zeroVelocity

                    else
                        firstDeriv.y / firstDeriv.x

        Wobble wobble ->
            zeroVelocity


type alias Domain =
    { start : Bezier.Point
    , end : Bezier.Point
    }


{-| -}
splines : Domain -> Float -> Float -> Transition -> List Bezier.Spline
splines domain introVelocity exitVelocity transition =
    case transition of
        Transition spline ->
            [ toDomain domain introVelocity exitVelocity spline ]

        Trail trail ->
            trailSplines domain introVelocity exitVelocity trail []

        Wobble wobble ->
            let
                params =
                    Spring.select wobble
                        (Quantity.Quantity (domain.end.x - domain.start.x))
            in
            Spring.segments params
                { position = domain.start.y
                , velocity = introVelocity
                }
                domain.end.y


trailSplines : Domain -> Float -> Float -> List Bezier.Spline -> List Bezier.Spline -> List Bezier.Spline
trailSplines domain introVelocity exitVelocity trail captured =
    case trail of
        [] ->
            captured

        spline :: [] ->
            toDomain domain introVelocity exitVelocity spline :: captured

        spline :: remain ->
            trailSplines domain
                0
                exitVelocity
                remain
                (toDomain domain introVelocity 0 spline
                    :: captured
                )


third : Float
third =
    1 / 3


negativeThird : Float
negativeThird =
    -1 / 3


{-| Note, we only rotate the control point to match the desired velocity.

However, there is the question of the magnitude of the control point.

I _think_ the magnitude is roughly equivalent to momentum.

It's possible that we override the built-in control points when there is a non-0 intro/exit Velocity.

Maybe it's a constant like 1/3 or something....

-}
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


{-| -}
hash : Float -> Float -> Float -> Float -> Float -> Transition -> String
hash start startVelocity end endVelocity t transition =
    case transition of
        Transition spline ->
            Bezier.hash spline

        Trail trail ->
            List.map Bezier.hash trail
                |> String.join "-"

        Wobble f ->
            "wob-" ++ String.fromFloat f


{-| -}
keyframes : Domain -> Float -> Float -> (Float -> String) -> Transition -> String
keyframes domain introVelocity exitVelocity toString transition =
    case transition of
        Transition spline ->
            let
                normalized =
                    spline
                        |> toDomain
                            { start =
                                { x = 0
                                , y = domain.start.y
                                }
                            , end =
                                { x = 1
                                , y = domain.end.y
                                }
                            }
                            introVelocity
                            exitVelocity
            in
            splineKeyframes toString normalized
                ++ finalFrame toString normalized

        Trail trail ->
            renderTrailKeyframes domain introVelocity exitVelocity toString trail ""

        Wobble wobble ->
            let
                params =
                    Spring.select wobble
                        (Quantity.Quantity (domain.end.x - domain.start.x))

                trail =
                    Spring.segments params
                        { position = domain.start.y
                        , velocity = introVelocity
                        }
                        domain.end.y
            in
            renderKeyframeList toString trail ""


splineKeyframes : (Float -> String) -> Bezier.Spline -> String
splineKeyframes toString spline =
    String.fromFloat (Bezier.firstX spline * 100)
        ++ "% {"
        ++ (toString (Bezier.firstY spline) ++ ";")
        ++ ("animation-timing-function:" ++ Bezier.cssTimingString spline ++ ";")
        ++ "}"


finalFrame : (Float -> String) -> Bezier.Spline -> String
finalFrame toString spline =
    "100% {" ++ (toString (Bezier.lastY spline) ++ ";}")


renderKeyframeList : (Float -> String) -> List Bezier.Spline -> String -> String
renderKeyframeList toString trail rendered =
    case trail of
        [] ->
            rendered

        spline :: [] ->
            rendered
                ++ splineKeyframes toString spline
                ++ finalFrame toString spline

        spline :: remain ->
            renderKeyframeList
                toString
                remain
                (splineKeyframes toString spline
                    ++ rendered
                )


renderTrailKeyframes : Domain -> Float -> Float -> (Float -> String) -> List Bezier.Spline -> String -> String
renderTrailKeyframes domain introVelocity exitVelocity toString trail rendered =
    case trail of
        [] ->
            rendered

        spline :: [] ->
            let
                normalized =
                    toDomain domain introVelocity exitVelocity spline
            in
            rendered
                ++ splineKeyframes toString normalized
                ++ finalFrame toString normalized

        spline :: remain ->
            renderTrailKeyframes domain
                0
                exitVelocity
                toString
                remain
                (splineKeyframes toString (toDomain domain introVelocity 0 spline)
                    ++ rendered
                )


{-|

    *Note* need to guarantee that the strings passed to toStr
    are given in the same order as defined in `transitions`

-}
compoundKeyframes : (List String -> String) -> List ( Float -> String, Transition ) -> String
compoundKeyframes toStr transitions =
    let
        onlyTransitions =
            List.map Tuple.second transitions
    in
    if isConflicting onlyTransitions onlyTransitions then
        renderCompoundKeyframesExact

    else
        renderCompoundKeyframes


isConflicting all reducing =
    case reducing of
        [] ->
            False

        trans :: remain ->
            if List.any (conflicting trans) all then
                True

            else
                isConflicting all remain


{-| -}
conflicting : Transition -> Transition -> Bool
conflicting one two =
    case one of
        Transition bezOne ->
            case two of
                Transition bezTwo ->
                    bezOne == bezTwo

                _ ->
                    True

        Trail _ ->
            False

        Wobble wobOne ->
            case two of
                Wobble wobTwo ->
                    wobOne - wobTwo == 0

                _ ->
                    True


renderCompoundKeyframesExact =
    ""


renderCompoundKeyframes =
    ""


{-| -}
before : Float -> Transition -> Transition
before t transition =
    transition


{-| -}
split : Float -> Transition -> { before : Transition, after : Transition }
split t transition =
    { before = transition
    , after = transition
    }
