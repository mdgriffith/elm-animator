module InternalAnim.Transition exposing
    ( Transition(..)
    , linear, standard, wobble, bezier
    , hash, keyframes
    , atX
    , isStandard
    , takeAfter, withVelocities
    )

{-|

@docs Transition

@docs linear, standard, wobble, bezier

@docs hash, keyframes

@docs atX

@docs isStandard

@docs takeAfter, withVelocities

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

import Bezier
import Bezier.Spring as Spring
import InternalAnim.Hash as Hash


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
    | Wobble
        { introVelocity : Float
        , wobble : Float
        , quickness : Float
        }


bezier : Float -> Float -> Float -> Float -> Transition
bezier one two three four =
    Transition <|
        Bezier.fromPoints
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
wobble : { wobble : Float, quickness : Float } -> Transition
wobble options =
    Wobble
        { introVelocity = 0
        , wobble = clamp 0 1 options.wobble
        , quickness = clamp 0 1 options.quickness
        }


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
        Bezier.standard


linear : Transition
linear =
    Transition <|
        Bezier.fromPoints
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
        Transition spline ->
            let
                { one, two, three, four } =
                    toBezierPoints spline
            in
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


{-| -}
atX :
    Float
    -> Transition
    ->
        { position : Bezier.Point
        , velocity : Bezier.Point
        }
atX progress transition =
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
            let
                totalX =
                    1

                params =
                    Spring.new
                        { wobble = wob.wobble
                        , quickness = wob.quickness
                        , settleMax = totalX
                        }

                sprung =
                    Spring.at
                        { spring = params
                        , initial =
                            { position = 0
                            , velocity = wob.introVelocity
                            }
                        , target = totalX * progress
                        }
                        1
            in
            { position =
                { x = progress
                , y = sprung.position
                }
            , velocity =
                { x = progress
                , y = sprung.velocity
                }
            }


withVelocities : Float -> Float -> Transition -> Transition
withVelocities intro exit transition =
    case transition of
        Transition spline ->
            Transition (Bezier.withVelocities intro exit spline)

        Wobble wob ->
            Wobble
                { wobble = wob.wobble
                , quickness = wob.quickness
                , introVelocity = intro
                }


toBezierPoints : Bezier.Spline -> { one : Bezier.Point, two : Bezier.Point, three : Bezier.Point, four : Bezier.Point }
toBezierPoints spline =
    { one = Bezier.first spline
    , two = Bezier.controlOne spline
    , three = Bezier.controlTwo spline
    , four = Bezier.last spline
    }


{-| -}
hash : Transition -> String
hash transition =
    case transition of
        Transition spline ->
            Hash.bezierNormalized spline

        Wobble f ->
            "wob" ++ Hash.float f.wobble


{-| -}
keyframes :
    (Float -> String)
    -> Float
    -> Float
    -> Transition
    -> String
keyframes interpolate startPercent endPercent transition =
    if startPercent == endPercent then
        ""

    else
        case transition of
            Transition spline ->
                kf startPercent (interpolate 0) spline
                    ++ (if endPercent == 100 then
                            kf endPercent (interpolate 1) spline

                        else
                            ""
                       )

            Wobble wob ->
                let
                    splines =
                        Spring.segments
                            -- Select a spring that will wobble and settle in 1000 milliseconds
                            (Spring.new
                                { wobble = wob.wobble
                                , quickness = wob.quickness
                                , settleMax =
                                    -- (Quantity.Quantity (endPercent - startPercent))
                                    1000

                                -- endPercent - startPercent
                                }
                            )
                            { position = 0

                            -- intro velocity
                            , velocity = wob.introVelocity

                            -- 0
                            -- NOTE: need to normalize introVelocity to the 0-1 domain
                            }
                            1000
                in
                keyframeListFromNonNormalizedBezier splines
                    interpolate
                    ""


{-|

    Some nuances for keyframes!

    1. animation-timing-function applies to the *upcoming* transition, so always needs to be on the preceeding keyframe
    2. Values always correspond to the *end* of the keyframe!
    3. We generally don't want to specify a value for the first keyframe value because this allows the browser to

-}
kf : Float -> String -> Bezier.Spline -> String
kf percent prop spline =
    if percent == 100 then
        String.fromInt (floor percent)
            ++ "% {"
            ++ prop
            ++ ";}"

    else
        String.fromInt (floor percent)
            ++ "% {"
            ++ prop
            ++ ";animation-timing-function:"
            ++ Bezier.toCss spline
            ++ ";}"


keyframeFromSpline : Float -> value -> (value -> String) -> Bezier.Spline -> String
keyframeFromSpline percent start toString spline =
    if percent == 100 then
        String.fromInt (floor percent)
            ++ "% {"
            ++ toString start
            ++ ";}"

    else
        String.fromInt (floor percent)
            ++ "% {"
            ++ toString start
            ++ ";animation-timing-function:"
            ++ Bezier.toCss spline
            ++ ";}"


{-| The beziers are mapped

    |      y: 0-1000  |
    |      x: 0-1     |
    v                 v
    |--------|--------|

We pass in a function that maps x -> "translateX(250px)"

-}
keyframeListFromNonNormalizedBezier :
    List Bezier.Spline
    -> (Float -> String)
    -> String
    -> String
keyframeListFromNonNormalizedBezier steps toString str =
    case steps of
        [] ->
            str

        top :: [] ->
            let
                percent =
                    (Bezier.first top |> .x) / 1000

                value =
                    ((Bezier.first top |> .y) / 1000)
                        |> toString

                normalizedSpline =
                    Bezier.normalize top

                finalPercent =
                    (Bezier.last top |> .x) / 1000

                finalValue =
                    Bezier.last top
                        |> .y
                        |> toString
            in
            str
                ++ keyframeFromSpline (percent * 100) value identity normalizedSpline
                ++ keyframeFromSpline (finalPercent * 100) finalValue identity normalizedSpline

        top :: remain ->
            let
                percent =
                    (Bezier.first top |> .x) / 1000

                value =
                    ((Bezier.first top |> .y) / 1000)
                        |> toString

                normalizedSpline =
                    Bezier.normalize top
            in
            keyframeListFromNonNormalizedBezier
                remain
                toString
                (str
                    ++ keyframeFromSpline (percent * 100) value identity normalizedSpline
                )


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
