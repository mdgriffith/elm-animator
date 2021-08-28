module Internal.Transition exposing
    ( Transition(..)
    , linear, standard, wobble, bezier
    , initialVelocity, atX
    , split, before
    , hash, keyframes
    , atX2, isStandard, takeAfter, withVelocities
    )

{-|

@docs Transition

@docs linear, standard, wobble, bezier

@docs initialVelocity, atX

@docs split, before

@docs hash, keyframes

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
    -> for rendering css stuff

Goals:

    1. Can we make it where this data is adjusted as little as possible.
    2. Can we pack the splines as tightly as possible
        Do they have to be floats?  Can they be Ints?  Can they by bit-encoded ints?
    3. Conversion to css @keyframes should be as fast as possible.

-}

import Internal.Bezier as Bezier
import Internal.Bits as Bits
import Internal.Spring as Spring
import Internal.Time as Time
import Internal.Units as Units
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
    | Wobble { introVelocity : Float, wobble : Float }


bezier : Float -> Float -> Float -> Float -> Transition
bezier one two three four =
    Transition <|
        Bezier.Spline
            { x = 0
            , y = 0
            }
            { x = one
            , y = two
            }
            { x = three
            , y = four
            }
            { x = 1
            , y = 1
            }


{-| -}
wobble : Float -> Transition
wobble w =
    Wobble { introVelocity = 0, wobble = clamp 0 1 w }


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


linear : Transition
linear =
    Transition <|
        Bezier.Spline
            { x = 0
            , y = 0
            }
            { x = 0.2
            , y = 0.2
            }
            { x = 0.8
            , y = 0.8
            }
            { x = 1
            , y = 1
            }


isStandard : Transition -> Bool
isStandard trans =
    case trans of
        Transition (Bezier.Spline one two three four) ->
            (one
                == { x = 0
                   , y = 0
                   }
            )
                && (two
                        == { x = 0.4
                           , y = 0
                           }
                   )
                && (three
                        == { x = 0.2
                           , y = 1
                           }
                   )
                && (four
                        == { x = 1
                           , y = 1
                           }
                   )

        _ ->
            False


type alias TimeDomain =
    { start : PointInTime
    , end : PointInTime
    }


type alias PointInTime =
    { x : Time.Absolute
    , y : Units.Pixels
    }


{-| -}
atX :
    Float
    -> TimeDomain
    -> Units.PixelsPerSecond
    -> Units.PixelsPerSecond
    -> Transition
    ->
        { position : Units.Pixels
        , velocity : Units.PixelsPerSecond
        }
atX progress domain introVelocity exitVelocity transition =
    case transition of
        Transition spline ->
            if domain.start.x == domain.end.x then
                { position = domain.end.y
                , velocity =
                    exitVelocity
                }

            else
                spline
                    |> inTimeDomain domain introVelocity exitVelocity
                    |> posVel (toTimeProgress domain progress)

        Wobble wob ->
            let
                totalX =
                    Time.inMilliseconds domain.end.x - Time.inMilliseconds domain.start.x

                params =
                    Spring.select wob.wobble
                        (Quantity.Quantity totalX)
            in
            Spring.analytical params
                (Quantity.Quantity (totalX * progress))
                (Units.inPixels domain.end.y)
                { position = Units.inPixels domain.start.y
                , velocity = Units.inPixelsPerMs introVelocity
                }
                |> wrapUnits


{-| -}
atX2 :
    Float
    -> Transition
    ->
        { position : Bezier.Point
        , velocity : Bezier.Point
        }
atX2 progress transition =
    case transition of
        Transition spline ->
            let
                pos =
                    Bezier.atX progress spline
            in
            { position = pos.point
            , velocity =
                Bezier.firstDerivative spline pos.t
            }

        Wobble wob ->
            -- let
            --     totalX =
            --         1
            --     params =
            --         Spring.select wob.wobble
            --             (Quantity.Quantity totalX)
            -- in
            -- Spring.analytical params
            --     (Quantity.Quantity (totalX * progress))
            --     1
            --     { position = 0
            --     , velocity = wob.introVelocity
            --     }
            --     |> wrapUnits
            Debug.todo "atX2 - Wobble"


withVelocities : Float -> Float -> Transition -> Transition
withVelocities intro exit transition =
    case transition of
        Transition spline ->
            Transition (Bezier.withVelocities intro exit spline)

        Wobble wob ->
            Wobble
                { wobble = wob.wobble
                , introVelocity = intro
                }


toTimeProgress :
    TimeDomain
    -> Float
    -> Float
toTimeProgress domain factor =
    let
        start =
            Time.inMilliseconds domain.start.x

        end =
            Time.inMilliseconds domain.end.x
    in
    ((end - start) * factor) + start


wrapUnits state =
    { position =
        Pixels.pixels state.position
    , velocity =
        Pixels.pixelsPerSecond (state.velocity * 1000)
    }


posVel :
    Float
    -> Bezier.Spline
    ->
        { position : Quantity.Quantity Float Pixels.Pixels
        , velocity : Quantity.Quantity Float Pixels.PixelsPerSecond
        }
posVel progress spline =
    let
        current =
            Bezier.atX progress spline

        firstDeriv =
            Bezier.firstDerivative spline current.t
    in
    { position =
        Pixels.pixels current.point.y
    , velocity =
        Pixels.pixelsPerSecond ((firstDeriv.y / firstDeriv.x) * 1000)
    }


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

        Wobble wob ->
            zeroVelocity


type alias Domain =
    { start : Bezier.Point
    , end : Bezier.Point
    }


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


inTimeDomain : TimeDomain -> Units.PixelsPerSecond -> Units.PixelsPerSecond -> Bezier.Spline -> Bezier.Spline
inTimeDomain domain introVelocity exitVelocity (Bezier.Spline one two three four) =
    let
        totalX =
            Time.inMilliseconds domain.end.x - Time.inMilliseconds domain.start.x

        totalY =
            Units.inPixels domain.end.y - Units.inPixels domain.start.y

        ctrl1 =
            let
                angle =
                    atan2 (Units.inPixelsPerMs introVelocity) 1
            in
            { x =
                (totalX * two.x) + Time.inMilliseconds domain.start.x
            , y =
                (totalY * two.y) + Units.inPixels domain.start.y
            }
                |> rotateAroundTimePoint angle domain.start

        ctrl2 =
            let
                angle =
                    atan2 (Units.inPixelsPerMs exitVelocity) 1
            in
            { x =
                (totalX * three.x) + Time.inMilliseconds domain.start.x
            , y =
                (totalY * three.y) + Units.inPixels domain.start.y
            }
                |> rotateAroundTimePoint angle domain.end
    in
    Bezier.Spline
        { x = Time.inMilliseconds domain.start.x
        , y = Units.inPixels domain.start.y
        }
        ctrl1
        ctrl2
        { x = Time.inMilliseconds domain.end.x
        , y = Units.inPixels domain.end.y
        }


{-| -}
rotateAroundTimePoint : Float -> PointInTime -> Bezier.Point -> Bezier.Point
rotateAroundTimePoint radians center point =
    let
        centerX =
            Time.inMilliseconds center.x

        centerY =
            Units.inPixels center.y
    in
    { x = cos radians * (point.x - centerX) - sin radians * (point.y - centerY) + centerX
    , y = sin radians * (point.x - centerX) + cos radians * (point.y - centerY) + centerY
    }


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
hash : Transition -> String
hash transition =
    case transition of
        Transition spline ->
            Bezier.hash spline

        Wobble f ->
            "wob-" ++ String.fromFloat f.wobble


{-| -}
keyframes : { start : value, end : value } -> Float -> Float -> (value -> String) -> Transition -> String
keyframes domain startPercent endPercent toString transition =
    case transition of
        Transition spline ->
            splineKeyframes
                startPercent
                domain.start
                toString
                spline

        -- ++ finalFrame toString normalized
        Wobble wob ->
            -- let
            --     params =
            --         Spring.select wob.wobble
            --             (Quantity.Quantity (domain.end.x - domain.start.x))
            --     trail =
            --         Spring.segments params
            --             { position = domain.start.y
            --             -- intro velocity
            --             , velocity = wob.introVelocity
            --             }
            --             domain.end.y
            -- in
            -- renderKeyframeList startPercent endPercent toString trail ""
            ""


splineKeyframes : Float -> value -> (value -> String) -> Bezier.Spline -> String
splineKeyframes percent start toString spline =
    String.fromInt (floor percent)
        ++ "% {"
        ++ toString start
        ++ ";animation-timing-function:"
        ++ Bezier.cssTimingString spline
        ++ ";}"


finalFrame : value -> (value -> String) -> String
finalFrame finalValue toString =
    "100% {" ++ toString finalValue ++ ";}"


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


takeAfter : Float -> Transition -> Transition
takeAfter t transition =
    case transition of
        Transition spline ->
            let
                ( _, afterT ) =
                    Bezier.splitAtX t spline
            in
            Transition (Bezier.normalize afterT)

        Wobble wob ->
            -- convert to splines and store s `Trail`
            -- Debug.todo "TODO: Transition.takeAfter Wobble"
            -- Does this actually change at all?
            Wobble wob
