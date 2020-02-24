module Animator.Css exposing
    ( animated
    , with
    , Attribute, opacity, height, width
    , fontSize, fontColor, wordSpacing, letterSpacing
    , backgroundColor
    , borderColor, borderRadius
    , transform, transformWith, rotate, scale, xy, xyz
    , rotating
    )

{-|

@docs animated

@docs with


# Properties

@docs Attribute, opacity, height, width


# Text

@docs fontSize, fontColor, wordSpacing, letterSpacing


# Background

@docs backgroundColor


# Border

@docs borderColor, borderRadius


# Transform

@docs transform, transformWith, rotate, scale, xy, xyz

@docs withRotation, withRotationAround, withScale, withScaleXY

-}

import Animator exposing (..)
import Color exposing (Color)
import Duration
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Keyed
import Internal.Interpolate as Interpolate
import Internal.Time as Time
import Internal.Timeline as Timeline
import Pixels


{-| -}
type Attribute event
    = ColorAttribute String (event -> Color)
    | Attribute String (event -> Float) (Float -> String)
    | Movement String (event -> Movement) (Float -> String)
    | TransformAttr TransformOptions (event -> Transform)


type alias TransformOptions =
    { origin : Origin
    , perspective : Int
    }


type Origin
    = Center


originToString : Origin -> String
originToString origin =
    case origin of
        Center ->
            "center"



{- ANIMATOR -}


{-| -}
with :
    (model -> Timeline state)
    -> (Timeline state -> model -> model)
    -> Animator model
    -> Animator model
with get set (Timeline.Animator isRunning updateModel) =
    Timeline.Animator
        (\model ->
            if isRunning model then
                True

            else
                let
                    tl =
                        get model
                in
                Timeline.hasChanged tl || Timeline.justInitialized tl
        )
        (\now model ->
            let
                newModel =
                    updateModel now model
            in
            set (Timeline.update now (get newModel)) newModel
        )



{- Example keyframes ->


   @keyframes identifier {
     0% { top: 0; }
     50% { top: 30px; left: 20px; }
     50% { top: 10px; }
     100% { top: 0; }
   }


-}


type alias Anim =
    { name : String
    , duration : Float
    , delay : Float
    , repeat : Repeat
    , timingFn : TimingFn
    , keyframes : String
    }


type Repeat
    = Loop
    | Repeat Int


type TimingFn
    = Linear


renderAnimations : List Anim -> String
renderAnimations animations =
    renderKeyframes "" animations
        ++ "\n."
        ++ (renderClassName "" animations ++ """{
            animation-name: """ ++ renderName "" animations ++ """;
            animation-delay: """ ++ renderDelay "" animations ++ """;
            animation-duration: """ ++ renderDuration "" animations ++ """;
            animation-timing-function: """ ++ renderTiming "" animations ++ """;
            animation-fill-mode: forwards;
            animation-iteration-count: """ ++ renderIterations "" animations ++ """;
        }
        """)


renderKeyframes : String -> List Anim -> String
renderKeyframes str anim =
    case anim of
        [] ->
            str

        top :: remain ->
            renderKeyframes (str ++ "\n" ++ top.keyframes) remain


renderClassName : String -> List Anim -> String
renderClassName str anim =
    case anim of
        [] ->
            str

        top :: [] ->
            if str == "" then
                top.name

            else
                str ++ top.name

        top :: next :: remaining ->
            renderClassName (str ++ top.name ++ "-") (next :: remaining)


renderName : String -> List Anim -> String
renderName str anim =
    case anim of
        [] ->
            str

        top :: [] ->
            if str == "" then
                top.name

            else
                str ++ top.name

        top :: next :: remaining ->
            renderName (str ++ top.name ++ ", ") (next :: remaining)


renderDelay : String -> List Anim -> String
renderDelay str anim =
    case anim of
        [] ->
            str

        top :: [] ->
            if str == "" then
                String.fromFloat top.delay ++ "ms"

            else
                str ++ (String.fromFloat top.delay ++ "ms")

        top :: next :: remaining ->
            renderDelay (str ++ String.fromFloat top.delay ++ "ms, ") (next :: remaining)


renderDuration : String -> List Anim -> String
renderDuration str anim =
    case anim of
        [] ->
            str

        top :: [] ->
            if str == "" then
                String.fromFloat top.duration ++ "ms"

            else
                str ++ String.fromFloat top.duration ++ "ms"

        top :: next :: remaining ->
            renderDuration (str ++ String.fromFloat top.duration ++ "ms, ") (next :: remaining)


renderTiming : String -> List Anim -> String
renderTiming str anim =
    case anim of
        [] ->
            str

        top :: [] ->
            if str == "" then
                timingFnName top.timingFn

            else
                str ++ timingFnName top.timingFn

        top :: next :: remaining ->
            renderTiming (str ++ timingFnName top.timingFn ++ ", ") (next :: remaining)


timingFnName : TimingFn -> String
timingFnName fn =
    case fn of
        Linear ->
            "linear"


renderIterations : String -> List Anim -> String
renderIterations str anim =
    case anim of
        [] ->
            str

        top :: [] ->
            if str == "" then
                repeatToString top.repeat

            else
                str ++ repeatToString top.repeat

        top :: next :: remaining ->
            renderIterations (str ++ repeatToString top.repeat ++ ", ") (next :: remaining)


repeatToString : Repeat -> String
repeatToString repeat =
    case repeat of
        Loop ->
            "infinite"

        Repeat n ->
            String.fromInt n


{-| -}
renderAttrs : Timeline event -> Attribute event -> List Anim -> List Anim
renderAttrs ((Timeline.Timeline details) as timeline) attr anim =
    case attr of
        ColorAttribute attrName lookup ->
            renderAnimation details.now
                attrName
                (Timeline.capture 60 lookup Interpolate.coloring timeline)
                Color.toCssString
                anim

        Attribute attrName lookup toString ->
            renderAnimation details.now
                attrName
                (Timeline.capture 60 lookup Interpolate.linearly timeline)
                toString
                anim

        Movement attrName lookup toString ->
            renderAnimation details.now
                attrName
                (Timeline.capture 60 lookup Interpolate.moving timeline)
                (.position >> Pixels.inPixels >> toString)
                anim

        TransformAttr options lookupTransform ->
            let
                lookup state =
                    case lookupTransform state of
                        Transform deets ->
                            deets

                x =
                    Timeline.capture 60 (lookup >> .x) Interpolate.moving timeline

                y =
                    Timeline.capture 60 (lookup >> .y) Interpolate.moving timeline

                z =
                    Timeline.capture 60 (lookup >> .z) Interpolate.moving timeline

                rotation =
                    Timeline.capture 60 (lookup >> .rotate) Interpolate.moving timeline

                scaleX =
                    Timeline.capture 60 (lookup >> .scaleX) Interpolate.moving timeline

                scaleY =
                    Timeline.capture 60 (lookup >> .scaleX) Interpolate.moving timeline

                combined =
                    toTransform []
                        x.frames
                        y.frames
                        z.frames
                        rotation.frames
                        scaleX.frames
                        scaleY.frames
            in
            renderAnimation details.now
                "transform"
                { frames =
                    List.reverse
                        combined
                , duration = x.duration
                , dwell =
                    Maybe.map (mapDwellFrames combined) rotation.dwell
                }
                transformToString
                anim


mapDwellFrames combinedFrames dwell =
    let
        lastFrame =
            case combinedFrames of
                [] ->
                    Timeline.Frame 1
                        { x = { position = Pixels.pixels 0, velocity = Pixels.pixelsPerSecond 0 }
                        , y = { position = Pixels.pixels 0, velocity = Pixels.pixelsPerSecond 0 }
                        , z = { position = Pixels.pixels 0, velocity = Pixels.pixelsPerSecond 0 }
                        , rotation = { position = Pixels.pixels 0, velocity = Pixels.pixelsPerSecond 0 }
                        , scaleX = { position = Pixels.pixels 1, velocity = Pixels.pixelsPerSecond 0 }
                        , scaleY = { position = Pixels.pixels 1, velocity = Pixels.pixelsPerSecond 0 }
                        }

                last :: _ ->
                    last
    in
    { period = dwell.period
    , frames = List.map (expand lastFrame) dwell.frames
    }


expand (Timeline.Frame _ lastFrame) (Timeline.Frame percent rotation) =
    Timeline.Frame percent
        { x = lastFrame.x
        , y = lastFrame.y
        , z = lastFrame.z
        , rotation = rotation
        , scaleX = lastFrame.scaleX
        , scaleY = lastFrame.scaleY
        }


toTransform rendered x y z rotation scaleX scaleY =
    case x of
        (Timeline.Frame percent topX) :: rX ->
            case y of
                (Timeline.Frame _ topY) :: rY ->
                    case z of
                        (Timeline.Frame _ topZ) :: rZ ->
                            case rotation of
                                (Timeline.Frame _ topR) :: rR ->
                                    case scaleX of
                                        (Timeline.Frame _ topSx) :: rsx ->
                                            case scaleY of
                                                (Timeline.Frame _ topSy) :: rsy ->
                                                    toTransform
                                                        (Timeline.Frame percent
                                                            { x = topX
                                                            , y = topY
                                                            , z = topZ
                                                            , rotation = topR
                                                            , scaleX = topSx
                                                            , scaleY = topSy
                                                            }
                                                            :: rendered
                                                        )
                                                        rX
                                                        rY
                                                        rZ
                                                        rR
                                                        rsx
                                                        rsy

                                                [] ->
                                                    rendered

                                        [] ->
                                            rendered

                                [] ->
                                    rendered

                        [] ->
                            rendered

                [] ->
                    rendered

        [] ->
            rendered


transformToString details =
    ("translate3d("
        ++ String.fromFloat (Pixels.inPixels details.x.position)
        ++ "px, "
        ++ String.fromFloat (Pixels.inPixels details.y.position)
        ++ "px, "
        ++ String.fromFloat (Pixels.inPixels details.z.position)
        ++ "px)"
    )
        ++ (" rotate(" ++ String.fromFloat (Pixels.inPixels details.rotation.position) ++ "rad)")
        ++ (" scale3d(" ++ String.fromFloat (Pixels.inPixels details.scaleX.position) ++ ", " ++ String.fromFloat (Pixels.inPixels details.scaleX.position) ++ ", 1)")


renderAnimation : Time.Absolute -> String -> Timeline.Frames value -> (value -> String) -> List Anim -> List Anim
renderAnimation now attrName frames renderer anims =
    let
        renderedFrames =
            renderFrame 0
                (List.length frames.frames)
                attrName
                frames.frames
                renderer
                ""

        name =
            attrName ++ "-" ++ String.fromInt (floor (Time.inMilliseconds now))

        newKeyFrames =
            "@keyframes " ++ name ++ " {\n" ++ renderedFrames ++ "\n}"

        duration =
            Duration.inMilliseconds frames.duration

        anim =
            { name = name
            , duration = duration
            , delay = 0
            , repeat = Repeat 1
            , timingFn = Linear
            , keyframes = newKeyFrames
            }
    in
    case frames.dwell of
        Nothing ->
            anim :: anims

        Just details ->
            let
                dwellFrames =
                    renderFrame 0
                        (List.length details.frames)
                        attrName
                        details.frames
                        renderer
                        ""

                dwell =
                    { name = name ++ "-dwell"
                    , duration =
                        case details.period of
                            Timeline.Repeat _ dwellDur ->
                                Duration.inMilliseconds dwellDur

                            Timeline.Loop dwellDur ->
                                Duration.inMilliseconds dwellDur
                    , delay = duration
                    , repeat =
                        case details.period of
                            Timeline.Repeat n _ ->
                                Repeat n

                            Timeline.Loop _ ->
                                Loop
                    , timingFn = Linear
                    , keyframes = "@keyframes " ++ name ++ "-dwell" ++ " {\n" ++ dwellFrames ++ "\n}"
                    }
            in
            dwell :: anim :: anims


{-| We always want to plot out frames so that 0% and 100% are present.

`i` always starts at 0

    i @ total = 1
        0/1 ->

-}
renderFrame : Int -> Int -> String -> List (Timeline.Frame value) -> (value -> String) -> String -> String
renderFrame i total name frames renderer rendered =
    case frames of
        [] ->
            rendered

        (Timeline.Frame percent frm) :: remain ->
            if total == 1 then
                let
                    keyframe =
                        ("0% {" ++ name ++ ": " ++ renderer frm ++ ";}\n")
                            ++ ("100% {" ++ name ++ ": " ++ renderer frm ++ ";}\n")
                in
                -- renderFrame (i + 1) total name remain renderer (rendered ++ keyframe)
                keyframe

            else
                let
                    keyframe =
                        String.fromFloat (100 * percent) ++ "% {" ++ name ++ ": " ++ renderer frm ++ ";}\n"
                in
                renderFrame (i + 1) total name remain renderer (rendered ++ keyframe)



-- keyframe
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


stylesheet : String -> Html msg
stylesheet str =
    Html.node "style"
        []
        [ Html.text str
        ]


{-| -}
animated :
    Timeline event
    -> List (Attribute event)
    -> List (Html.Attribute msg)
    -> List (Html msg)
    -> Html msg
animated timeline animatedAttrs attrs children =
    let
        animations =
            List.foldl (renderAttrs timeline) [] animatedAttrs
                |> List.reverse

        transformOptions =
            getTransformOptions animatedAttrs
    in
    Html.Keyed.node "div"
        attrs
        [ ( "animator-stylesheet", stylesheet (renderAnimations animations) )
        , ( "animated-node"
          , Html.div (Attr.class (renderClassName "" animations) :: attrs) children
          )
        ]


getTransformOptions attrs =
    case attrs of
        [] ->
            [ Attr.style "perspective" "20px"
            , Attr.style "transform-origin" "center"
            ]

        (TransformAttr opts _) :: _ ->
            [ Attr.style "perspective" (String.fromInt opts.perspective ++ "px")
            , Attr.style "transform-origin" (originToString opts.origin)
            ]

        _ :: rest ->
            getTransformOptions rest


px : Float -> String
px f =
    String.fromFloat f ++ "px"


{-| -}
opacity : (event -> Float) -> Attribute event
opacity lookup =
    Attribute
        "opacity"
        lookup
        String.fromFloat


{-| -}
width : (event -> Movement) -> Attribute event
width lookup =
    Movement "width" lookup px


{-| -}
height : (event -> Movement) -> Attribute event
height lookup =
    Movement "height" lookup px



{- Text Attributes -}


{-| -}
fontColor : (event -> Color.Color) -> Attribute event
fontColor lookup =
    ColorAttribute "color" lookup


{-| -}
fontSize : (event -> Movement) -> Attribute event
fontSize lookup =
    Movement "font-size" lookup px


{-| -}
wordSpacing : (event -> Movement) -> Attribute event
wordSpacing lookup =
    Movement "word-spacing" lookup px


{-| -}
letterSpacing : (event -> Movement) -> Attribute event
letterSpacing lookup =
    Movement "letter-spacing" lookup px



{- BACKGROUND -}
-- {-| -}
-- backgroundXy :
--     Timeline state
--     ->
--         (state
--          ->
--             { x : Movement
--             , y : Movement
--             }
--         )
--    -> Attribute event
-- backgroundXy timeline lookup =
--     { x =
--         Timeline.foldp
--             (lookup >> .x)
--             Interpolate.moving
--             timeline
--             |> unwrapUnits
--             |> .position
--     , y =
--         Timeline.foldp
--             (lookup >> .y)
--             Interpolate.moving
--             timeline
--             |> unwrapUnits
--             |> .position
--     }


{-| -}
backgroundColor : (event -> Color.Color) -> Attribute event
backgroundColor lookup =
    ColorAttribute "background-color" lookup



{- BORDERS -}


{-| -}
borderColor : (event -> Color.Color) -> Attribute event
borderColor lookup =
    ColorAttribute "border-color" lookup


{-| -}
borderRadius : (event -> Movement) -> Attribute event
borderRadius lookup =
    Movement "border-radius" lookup px



{- TRANSFORMS -}


{-| Rotate. An animation where the values are increasing will move clockwise.

The actual number provided is in Elm standard angles (radians).

Alternatively you could use [turns or degrees](https://package.elm-lang.org/packages/elm/core/latest/Basics#degrees) instead.

-}
rotate : Float -> Transform
rotate angle =
    Transform
        { x = at 0
        , y = at 0
        , z = at 0
        , rotate = at angle
        , scaleX = at 1
        , scaleY = at 1
        }


{-| -}
scale : Float -> Transform
scale movement =
    Transform
        { x = at 0
        , y = at 0
        , z = at 0
        , rotate = at 0
        , scaleX = at movement
        , scaleY = at movement
        }


{-| -}
xy :
    { x : Float
    , y : Float
    }
    -> Transform
xy coords =
    Transform
        { x = at coords.x
        , y = at coords.y
        , z = at 0
        , rotate = at 0
        , scaleX = at 1
        , scaleY = at 1
        }


{-| -}
xyz :
    { x : Float
    , y : Float
    , z : Float
    }
    -> Transform
xyz coords =
    Transform
        { x = at coords.x
        , y = at coords.y
        , z = at coords.z
        , rotate = at 0
        , scaleX = at 1
        , scaleY = at 1
        }



-- {-| -}
-- withScale : Float -> Transform -> Transform
-- withScale movement (Transform trans) =
--     Transform
--         { trans
--             | scaleX = at movement
--             , scaleY = at movement
--         }
-- {-| -}
-- withScaleXY : Float -> Float -> Transform -> Transform
-- withScaleXY x y (Transform trans) =
--     Transform
--         { trans
--             | scaleX = at x
--             , scaleY = at y
--         }


{-| Provide the duration it should take for one full rotation.
-}
rotating : Duration -> Transform -> Transform
rotating dur (Transform trans) =
    Transform
        { trans
            | rotate =
                loop dur (wrap 0 (2 * pi))
        }



-- {-| -}
-- withRotationAround :
--     { x : Float
--     , y : Float
--     , z : Float
--     , angle : Float
--     }
--     -> Transform
--     -> Transform
-- withRotationAround coords (Transform trans) =
--     Transform
--         trans


{-| -}
type Transform
    = Transform
        { x : Movement
        , y : Movement
        , z : Movement
        , rotate : Movement
        , scaleX : Movement
        , scaleY : Movement
        }


{-| -}
transform : (state -> Transform) -> Attribute state
transform =
    TransformAttr defaultTransformOpts


{-| -}
transformWith : TransformOptions -> (state -> Transform) -> Attribute state
transformWith =
    TransformAttr


{-| -}
defaultTransformOpts : TransformOptions
defaultTransformOpts =
    { origin = Center
    , perspective = 10
    }
