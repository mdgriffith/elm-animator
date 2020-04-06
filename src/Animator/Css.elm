module Animator.Css exposing
    ( watching
    , div, node
    , Attribute, opacity, height, width
    , fontSize, fontColor, wordSpacing, letterSpacing
    , backgroundColor
    , borderColor, borderRadius
    , style, color
    , explain
    , Transform, transform
    , rotateTo, lookAt, rotating
    , scale, xy, xyz
    , transformWith, TransformOptions, Origin, center, offset
    , resting
    , in2d, in3d
    , once, repeat, loop
    )

{-| Animate with CSS keyframes.

This can be a very performant because

1.  CSS animations are executed efficiently by the browser.

2.  We only have to ask for **one** `AnimationFrame` from the browser when an animation starts and we can render the entire animation.

This means `Elm` only needs to run your view code once instead of 60 times a second.

`Elm` is generally pretty fast and efficient at all this! But it can be even faster to **skip the work altogether**.

@docs watching

@docs div, node


# Properties

@docs Attribute, opacity, height, width


# Text

@docs fontSize, fontColor, wordSpacing, letterSpacing


# Background

@docs backgroundColor


# Border

@docs borderColor, borderRadius


# Custom

@docs style, color

**Note on Shadows and Filters** - You might be wondering why there is no `box-shadow` or `filter`.

They're not supported first class because they're expensive by default.

If you're trying to animate a `box-shadow`, your best bet is to render the box shadow once in a separate element, and then only animate opacity on that element.

Basically you want to recreate what is [described in this overview.](https://tobiasahlin.com/blog/how-to-animate-box-shadow/)


# Transform

@docs explain

@docs Transform, transform

@docs rotateTo, lookAt, rotating

@docs scale, xy, xyz


# Transform Options

@docs transformWith, TransformOptions, Origin, center, offset


# Advanced

**Note** - One of the difficulties in making an animation library for `CSS` is that operations such as `translate` and `rotate` need to be rendered as one property called `transform`.

While normally this isn't that big of a deal, one consequence for generating `CSS` keyframes is that all resting states for all transformations need to share the same `period`.

Because of that, `in2d` and `in3d` are constructed a little differently instead of just using `Movement` like everything else.

@docs resting

@docs in2d, in3d

**Note** - Again, because of the CSS characteristic mentioned above, we have to have `once` and `repeat` defined separetely from `Animator.once` for the special case of transforms.

@docs once, repeat, loop

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
type Attribute state
    = ColorAttribute String (state -> Color)
    | Linear String (state -> Movement) (Float -> String)
    | Movement String (state -> Movement) (Float -> String)
    | TransformAttr TransformOptions (state -> Transform)
    | Explain Bool


{-| This is a _debug_ tool for transformations.

If you turn this on, it will show you:

  - A grey bounding box so you get a sense of the element you're animating.

  - A **red dot** at the center of the element. Any rotation will be around this point.

    _You can adjust the dot's position using [`transformWith`](#transformWith) and setting a new origin._

  - The coordinate axes. Yes, in _CSS_, y points down.

![](https://mdgriffith.github.io/elm-animator/images/explain-example.png)

-}
explain : Bool -> Attribute state
explain =
    Explain


{-| -}
type alias TransformOptions =
    { rotationAxis :
        { x : Float
        , y : Float
        , z : Float
        }
    , origin : Origin
    }


{-| The origin for rotation. Defaults to the center of the element.
-}
type Origin
    = Center
    | Offset Float Float


{-| -}
center : Origin
center =
    Center


{-| This is the x and y offset, in pixels from the center of the object.

Generally I found the most common case for adjusting the origin was when you want it mostly centered, but aligned to the visual center of an icon or image, which is offset by a number of pixels.

-}
offset : Float -> Float -> Origin
offset =
    Offset



{- ANIMATOR -}


{-| `Animator.Css.watching` is different from `Animator.watching` in that it will only ask for one frame when an animation is updated.

In that one frame, we render the **entire CSS animation**, which can run without `Elm` needing to do a full rerender.

-}
watching :
    (model -> Timeline state)
    -> (Timeline state -> model -> model)
    -> Animator model
    -> Animator model
watching get set (Timeline.Animator isRunning updateModel) =
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


type alias Anim =
    { name : String
    , duration : Float
    , delay : Float
    , repeat : Repeat
    , timingFn : TimingFn
    , keyframes : String
    }


type Repeat
    = LoopAnim
    | RepeatAnim Int


type TimingFn
    = LinearTiming


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
        LinearTiming ->
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
repeatToString rep =
    case rep of
        LoopAnim ->
            "infinite"

        RepeatAnim n ->
            String.fromInt n


stubColor : Color.Color -> String
stubColor clr =
    case Color.toRgba clr of
        { red, green, blue, alpha } ->
            (stubFloat red ++ "-")
                ++ (stubFloat green ++ "-")
                ++ (stubFloat blue ++ "-")
                ++ stubFloat alpha


stubFloat : Float -> String
stubFloat f =
    String.fromInt (round (f * 1000))


{-| -}
renderAttrs : Timeline state -> Attribute state -> List Anim -> List Anim
renderAttrs ((Timeline.Timeline details) as timeline) attr anim =
    case attr of
        Explain _ ->
            anim

        ColorAttribute attrName lookup ->
            renderAnimation details.now
                attrName
                (Timeline.capture 60 lookup Interpolate.coloring timeline)
                Color.toCssString
                stubColor
                anim

        Linear attrName lookup toString ->
            renderAnimation details.now
                attrName
                (Timeline.capture 60 (lookup >> Interpolate.withLinearDefault) Interpolate.moving timeline)
                (.position >> Pixels.inPixels >> toString)
                (.position >> Pixels.inPixels >> stubFloat)
                anim

        Movement attrName lookup toString ->
            renderAnimation details.now
                attrName
                (Timeline.capture 60 (lookup >> Interpolate.withStandardDefault) Interpolate.moving timeline)
                (.position >> Pixels.inPixels >> toString)
                (.position >> Pixels.inPixels >> stubFloat)
                anim

        TransformAttr options lookupTransform ->
            let
                lookup state =
                    case lookupTransform state of
                        Transform deets ->
                            deets

                x =
                    Timeline.capture 60 (lookup >> .x >> Interpolate.withStandardDefault) Interpolate.moving timeline

                y =
                    Timeline.capture 60 (lookup >> .y >> Interpolate.withStandardDefault) Interpolate.moving timeline

                z =
                    Timeline.capture 60 (lookup >> .z >> Interpolate.withStandardDefault) Interpolate.moving timeline

                rotation =
                    Timeline.capture 60 (lookup >> .rotate >> Interpolate.withLinearDefault) Interpolate.moving timeline

                scaleX =
                    Timeline.capture 60 (lookup >> .scaleX >> Interpolate.withStandardDefault) Interpolate.moving timeline

                scaleY =
                    Timeline.capture 60 (lookup >> .scaleY >> Interpolate.withStandardDefault) Interpolate.moving timeline

                scaleZ =
                    Timeline.capture 60 (lookup >> .scaleZ >> Interpolate.withStandardDefault) Interpolate.moving timeline

                facingX =
                    Timeline.capture 60 (lookup >> .facing >> .x >> Interpolate.withStandardDefault) Interpolate.moving timeline

                facingY =
                    Timeline.capture 60 (lookup >> .facing >> .y >> Interpolate.withStandardDefault) Interpolate.moving timeline

                facingZ =
                    Timeline.capture 60 (lookup >> .facing >> .z >> Interpolate.withStandardDefault) Interpolate.moving timeline

                combined =
                    toTransform options
                        []
                        x.frames
                        y.frames
                        z.frames
                        rotation.frames
                        scaleX.frames
                        scaleY.frames
                        scaleZ.frames
                        facingX.frames
                        facingY.frames
                        facingZ.frames
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
                transformToStub
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
                        , scaleZ = { position = Pixels.pixels 1, velocity = Pixels.pixelsPerSecond 0 }
                        , aroundX = 0
                        , aroundY = 0
                        , aroundZ = 1
                        , facingX = { position = Pixels.pixels 0, velocity = Pixels.pixelsPerSecond 0 }
                        , facingY = { position = Pixels.pixels 0, velocity = Pixels.pixelsPerSecond 0 }
                        , facingZ = { position = Pixels.pixels 1, velocity = Pixels.pixelsPerSecond 0 }
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
        , scaleZ = lastFrame.scaleZ
        , aroundX = lastFrame.aroundX
        , aroundY = lastFrame.aroundY
        , aroundZ = lastFrame.aroundZ
        , facingX = lastFrame.facingX
        , facingY = lastFrame.facingY
        , facingZ = lastFrame.facingZ
        }


toTransform options rendered x y z rotation scaleX scaleY scaleZ facingX facingY facingZ =
    -- get ready for a tornado...
    case x of
        [] ->
            rendered

        (Timeline.Frame percent topX) :: rX ->
            case y of
                [] ->
                    rendered

                (Timeline.Frame _ topY) :: rY ->
                    case z of
                        [] ->
                            rendered

                        (Timeline.Frame _ topZ) :: rZ ->
                            case rotation of
                                [] ->
                                    rendered

                                (Timeline.Frame _ topR) :: rR ->
                                    case scaleX of
                                        [] ->
                                            rendered

                                        (Timeline.Frame _ topSx) :: rsx ->
                                            case scaleY of
                                                [] ->
                                                    rendered

                                                (Timeline.Frame _ topSy) :: rsy ->
                                                    case scaleZ of
                                                        [] ->
                                                            rendered

                                                        (Timeline.Frame _ topSz) :: rsz ->
                                                            case facingX of
                                                                [] ->
                                                                    rendered

                                                                (Timeline.Frame _ topFx) :: rfx ->
                                                                    case facingY of
                                                                        [] ->
                                                                            rendered

                                                                        (Timeline.Frame _ topFy) :: rfy ->
                                                                            case facingZ of
                                                                                [] ->
                                                                                    rendered

                                                                                (Timeline.Frame _ topFz) :: rfz ->
                                                                                    toTransform options
                                                                                        (Timeline.Frame percent
                                                                                            { x = topX
                                                                                            , y = topY
                                                                                            , z = topZ
                                                                                            , rotation = topR
                                                                                            , scaleX = topSx
                                                                                            , scaleY = topSy
                                                                                            , scaleZ = topSz
                                                                                            , aroundX = options.rotationAxis.x
                                                                                            , aroundY = options.rotationAxis.y
                                                                                            , aroundZ = options.rotationAxis.z
                                                                                            , facingX = topFx
                                                                                            , facingY = topFy
                                                                                            , facingZ = topFz
                                                                                            }
                                                                                            :: rendered
                                                                                        )
                                                                                        rX
                                                                                        rY
                                                                                        rZ
                                                                                        rR
                                                                                        rsx
                                                                                        rsy
                                                                                        rsz
                                                                                        rfx
                                                                                        rfy
                                                                                        rfz


toState p =
    { position = Pixels.pixels p
    , velocity = Pixels.pixelsPerSecond 0
    }


transformToString details =
    let
        ( _, rotationAroundY ) =
            toPolar
                ( Pixels.inPixels details.facingZ.position
                , Pixels.inPixels details.facingX.position
                )

        ( _, rotationAroundX ) =
            toPolar
                ( Pixels.inPixels details.facingZ.position
                , Pixels.inPixels details.facingY.position
                )
    in
    ("translate3d("
        ++ String.fromFloat (Pixels.inPixels details.x.position)
        ++ "px, "
        ++ String.fromFloat (Pixels.inPixels details.y.position)
        ++ "px, "
        ++ String.fromFloat (Pixels.inPixels details.z.position)
        ++ "px)"
    )
        ++ (" rotate3d(0, 1, 0, "
                ++ String.fromFloat rotationAroundY
                ++ "rad)"
           )
        ++ (" rotate3d(1, 0, 0, "
                ++ String.fromFloat rotationAroundX
                ++ "rad)"
           )
        ++ (" rotate3d("
                ++ String.fromFloat details.aroundX
                ++ ", "
                ++ String.fromFloat details.aroundY
                ++ ", "
                ++ String.fromFloat details.aroundZ
                ++ ", "
                ++ String.fromFloat (Pixels.inPixels details.rotation.position)
                ++ "rad)"
           )
        ++ (" scale3d("
                ++ String.fromFloat (Pixels.inPixels details.scaleX.position)
                ++ ", "
                ++ String.fromFloat (Pixels.inPixels details.scaleY.position)
                ++ ", "
                ++ String.fromFloat (Pixels.inPixels details.scaleZ.position)
                ++ ")"
           )


transformToStub details =
    let
        ( _, rotationAroundY ) =
            toPolar
                ( Pixels.inPixels details.facingZ.position
                , Pixels.inPixels details.facingX.position
                )

        ( _, rotationAroundX ) =
            toPolar
                ( Pixels.inPixels details.facingZ.position
                , Pixels.inPixels details.facingY.position
                )
    in
    stubFloat (Pixels.inPixels details.x.position)
        ++ "-"
        ++ stubFloat (Pixels.inPixels details.y.position)
        ++ "-"
        ++ stubFloat (Pixels.inPixels details.z.position)
        ++ "-"
        ++ stubFloat rotationAroundY
        ++ "-"
        ++ stubFloat rotationAroundX
        ++ "-"
        ++ stubFloat details.aroundX
        ++ "-"
        ++ stubFloat details.aroundY
        ++ "-"
        ++ stubFloat details.aroundZ
        ++ "-"
        ++ stubFloat (Pixels.inPixels details.rotation.position)
        ++ "-"
        ++ stubFloat (Pixels.inPixels details.scaleX.position)
        ++ "-"
        ++ stubFloat (Pixels.inPixels details.scaleY.position)
        ++ "-"
        ++ stubFloat (Pixels.inPixels details.scaleZ.position)


renderAnimation : Time.Absolute -> String -> Timeline.FramesSummary value -> (value -> String) -> (value -> String) -> List Anim -> List Anim
renderAnimation now attrName frames renderer stubber anims =
    let
        rendered =
            renderFrame 0
                (List.length frames.frames)
                attrName
                frames.frames
                renderer
                stubber
                ""
                ""

        name =
            attrName
                ++ "-"
                ++ String.fromInt (floor (Time.inMilliseconds now))
                ++ "-"
                ++ rendered.uniqueName

        newKeyFrames =
            "@keyframes " ++ name ++ " {\n" ++ rendered.frames ++ "\n}"

        duration =
            Duration.inMilliseconds frames.duration

        anim =
            { name = name
            , duration = duration
            , delay = 0
            , repeat = RepeatAnim 1
            , timingFn = LinearTiming
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
                        stubber
                        ""
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
                                RepeatAnim n

                            Timeline.Loop _ ->
                                LoopAnim
                    , timingFn = LinearTiming
                    , keyframes = "@keyframes " ++ name ++ "-dwell" ++ " {\n" ++ dwellFrames.frames ++ "\n}"
                    }
            in
            dwell :: anim :: anims


{-| We always want to plot out frames so that 0% and 100% are present.

`i` always starts at 0

    i @ total = 1
        0/1 ->

-}
renderFrame :
    Int
    -> Int
    -> String
    -> List (Timeline.Frame value)
    -> (value -> String)
    -> (value -> String)
    -> String
    -> String
    ->
        { frames : String
        , uniqueName : String
        }
renderFrame i total name frames renderer stubber rendered stub =
    case frames of
        [] ->
            { frames = rendered
            , uniqueName = stub
            }

        (Timeline.Frame percent frm) :: remain ->
            if total == 1 then
                let
                    keyframe =
                        ("0% {" ++ name ++ ": " ++ renderer frm ++ ";}\n")
                            ++ ("100% {" ++ name ++ ": " ++ renderer frm ++ ";}\n")
                in
                { frames = keyframe
                , uniqueName = stubber frm
                }

            else
                let
                    keyframe =
                        String.fromFloat (100 * percent) ++ "% {" ++ name ++ ": " ++ renderer frm ++ ";}\n"
                in
                renderFrame (i + 1) total name remain renderer stubber (rendered ++ keyframe) (stub ++ stubber frm)



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


{-| This is a single div element.

It's just like a normal `Html` node, except it also takes a `Timeline` and a list of attributes you want to animate.

Here's a checkbox that changes backgrounds as a brief example:

    Animator.Css.div model.checked
        [ Animator.Css.backgroundColor <|
            \checked ->
                if checked then
                    Color.rgb255 255 96 96

                else
                    Color.white
        ]
        [ Attr.style "height" "30px"
        , Attr.style "width" "30px"
        ]
        [ Html.text "" ]

-}
div :
    Timeline state
    -> List (Attribute state)
    -> List (Html.Attribute msg)
    -> List (Html msg)
    -> Html msg
div =
    node "div"


{-| Specify a node name that's not a div. Here's an `<a>`.

    Animator.Css.node "a"
        model.checked
        [ Animator.Css.backgroundColor <|
            \checked ->
                if checked then
                    Color.rgb255 255 96 96

                else
                    Color.white
        ]
        [ Attr.style "height" "30px"
        , Attr.style "width" "30px"
        ]
        [ Html.text "" ]

-}
node :
    String
    -> Timeline state
    -> List (Attribute state)
    -> List (Html.Attribute msg)
    -> List (Html msg)
    -> Html msg
node name timeline divAttrs attrs children =
    let
        animations =
            List.foldl (renderAttrs timeline) [] divAttrs
                |> List.reverse

        transformOptions =
            getTransformOptions divAttrs
                |> Maybe.withDefault defaultTransformOptions

        explainIsOn =
            explainActive divAttrs

        possiblyExplainAttr =
            if explainIsOn then
                Attr.style "position" "relative"

            else
                Attr.style "" ""
    in
    -- Html.Keyed.node "div"
    --     []
    --     [ ( "animator-stylesheet", stylesheet (renderAnimations animations) )
    --     , ( "div-node"
    --       , Html.div (Attr.class (renderClassName "" animations) :: attrs) children
    --       )
    --     ]
    Html.node name
        (Attr.class (renderClassName "" animations)
            :: possiblyExplainAttr
            :: renderTransformOptions transformOptions
            :: attrs
        )
        (stylesheet (renderAnimations animations)
            :: (if explainIsOn then
                    explainElement transformOptions

                else
                    Html.text ""
               )
            :: children
        )


maybeAttr fn a =
    case Maybe.map fn a of
        Nothing ->
            Attr.style "" ""

        Just attr ->
            attr


{-| Explain shows:

-- bounding box for element, with current lookAt and

-}
explainElement : TransformOptions -> Html msg
explainElement transformOptions =
    Html.div
        [ Attr.style "position" "absolute"
        , Attr.style "width" "100%"
        , Attr.style "height" "100%"
        , Attr.style "background-color" "rgba(238, 238, 238, 0.4)"
        , Attr.style "border" "2px solid #DDD"
        , Attr.style "z-index" "10"
        ]
        [ viewAxes transformOptions
        ]


originDot : Float -> Float -> Html msg
originDot x y =
    Html.div
        [ Attr.style "position" "absolute"
        , Attr.style "width" "6px"
        , Attr.style "height" "6px"
        , Attr.style "left" ("calc(50% + " ++ String.fromFloat x ++ "px - 3px)")
        , Attr.style "top" ("calc(50% + " ++ String.fromFloat y ++ "px - 3px)")
        , Attr.style "background-color" "red"
        , Attr.style "border-radius" "3px"
        ]
        []


viewAxes : TransformOptions -> Html msg
viewAxes options =
    let
        ( x, y ) =
            case options.origin of
                Center ->
                    Tuple.pair 0 0

                Offset ox oy ->
                    Tuple.pair ox oy
    in
    Html.div
        [ Attr.style "position" "absolute"
        , Attr.style "width" "0px"
        , Attr.style "height" "0px"
        , Attr.style "left" ("calc(50% + " ++ String.fromFloat x ++ "px - 3px)")
        , Attr.style "top" ("calc(50% + " ++ String.fromFloat y ++ "px - 3px)")
        ]
        [ Html.div
            [ Attr.style "position" "absolute"
            , Attr.style "top" "10px"
            , Attr.style "left" "-1px"
            , Attr.style "width" "2px"
            , Attr.style "height" "50px"
            , Attr.style "background-color" "black"
            ]
            [ Html.div
                [ Attr.style "position" "absolute"
                , Attr.style "bottom" "-5px"
                , Attr.style "left" "-3px"
                , Attr.style "width" "0"
                , Attr.style "height" "0"
                , Attr.style "border-left" "4px solid transparent"
                , Attr.style "border-right" "4px solid transparent"
                , Attr.style "border-top" "8px solid black"
                ]
                []
            , Html.div
                [ Attr.style "position" "absolute"
                , Attr.style "right" "-3px"
                , Attr.style "bottom" "-22px"
                , Attr.style "font-size" "12px"
                ]
                [ Html.text "Y"
                ]
            ]
        , Html.div
            [ Attr.style "position" "absolute"
            , Attr.style "top" "-1px"
            , Attr.style "left" "10px"
            , Attr.style "width" "50px"
            , Attr.style "height" "2px"
            , Attr.style "background-color" "black"
            ]
            [ Html.div
                [ Attr.style "position" "absolute"
                , Attr.style "right" "-5px"
                , Attr.style "top" "-3px"
                , Attr.style "width" "0"
                , Attr.style "height" "0"
                , Attr.style "border-top" "4px solid transparent"
                , Attr.style "border-bottom" "4px solid transparent"
                , Attr.style "border-left" "8px solid black"
                ]
                []
            , Html.div
                [ Attr.style "position" "absolute"
                , Attr.style "right" "-14px"
                , Attr.style "top" "-6px"
                , Attr.style "font-size" "12px"
                ]
                [ Html.text "X"
                ]
            ]
        , Html.div
            [ Attr.style "position" "absolute"
            , Attr.style "width" "6px"
            , Attr.style "height" "6px"
            , Attr.style "left" "-3px"
            , Attr.style "top" "-3px"
            , Attr.style "background-color" "red"
            , Attr.style "border-radius" "3px"
            ]
            []
        , Html.div
            [ Attr.style "position" "absolute"
            , Attr.style "width" "12px"
            , Attr.style "height" "12px"
            , Attr.style "left" "-7px"
            , Attr.style "top" "-7px"
            , Attr.style "background-color" "transparent"
            , Attr.style "border-radius" "7px"
            , Attr.style "border" "1px solid red"
            ]
            []
        ]


explainActive : List (Attribute state) -> Bool
explainActive attrs =
    case attrs of
        [] ->
            False

        (Explain on) :: others ->
            on

        skip :: others ->
            explainActive others


renderTransformOptions : TransformOptions -> Html.Attribute msg
renderTransformOptions opts =
    Attr.style "transform-origin"
        (case opts.origin of
            Center ->
                "center"

            Offset x y ->
                ("calc(50% + " ++ String.fromFloat x ++ "px) calc(50% + ")
                    ++ (String.fromFloat y ++ "px)")
        )


getTransformOptions : List (Attribute state) -> Maybe TransformOptions
getTransformOptions attrs =
    case attrs of
        [] ->
            Nothing

        (TransformAttr opts _) :: _ ->
            Just opts

        _ :: rest ->
            getTransformOptions rest


px : Float -> String
px f =
    String.fromFloat f ++ "px"


{-| -}
style : String -> (Float -> String) -> (state -> Movement) -> Attribute state
style name toString lookup =
    Movement name lookup toString


{-| -}
color : String -> (state -> Color.Color) -> Attribute state
color =
    ColorAttribute


{-| -}
opacity : (state -> Movement) -> Attribute state
opacity lookup =
    Linear "opacity" lookup String.fromFloat


{-| -}
width : (state -> Movement) -> Attribute state
width lookup =
    Movement "width" lookup px


{-| -}
height : (state -> Movement) -> Attribute state
height lookup =
    Movement "height" lookup px



{- Text Attributes -}


{-| -}
fontColor : (state -> Color.Color) -> Attribute state
fontColor lookup =
    ColorAttribute "color" lookup


{-| -}
fontSize : (state -> Movement) -> Attribute state
fontSize lookup =
    Movement "font-size" lookup px


{-| -}
wordSpacing : (state -> Movement) -> Attribute state
wordSpacing lookup =
    Movement "word-spacing" lookup px


{-| -}
letterSpacing : (state -> Movement) -> Attribute state
letterSpacing lookup =
    Movement "letter-spacing" lookup px



{- BACKGROUND -}


{-| -}
backgroundColor : (state -> Color.Color) -> Attribute state
backgroundColor lookup =
    ColorAttribute "background-color" lookup



{- BORDERS -}


{-| -}
borderColor : (state -> Color.Color) -> Attribute state
borderColor lookup =
    ColorAttribute "border-color" lookup


{-| -}
borderRadius : (state -> Movement) -> Attribute state
borderRadius lookup =
    Movement "border-radius" lookup px



{- TRANSFORMS -}


{-| This represents moving something around in 3d.

This includes:

  - scaling
  - changing position
  - rotation

-}
type Transform
    = Transform
        { x : Movement
        , y : Movement
        , z : Movement
        , rotate : Movement
        , facing :
            { x : Movement
            , y : Movement
            , z : Movement
            }
        , scaleX : Movement
        , scaleY : Movement
        , scaleZ : Movement
        }


{-|

    Animator.Css.transform <|
        \state ->
            case state of
                Stationary ->
                    Animator.Css.xy
                        { x = 0
                        , y = 0
                        }

                Rotating ->
                    -- do a full rotation every 5 seconds
                    Animator.Css.rotating
                        (Animator.seconds 5)

**Note** - If you're doing 3d transformations, you should set a CSS `perspective` property either on this element or on a parent. If it's on a parent, then all elements will share a common `perspective` origin.

So, add something like this to the parent element:

    perspective: 500px;
    perspective-origin: center;

-}
transform : (state -> Transform) -> Attribute state
transform =
    TransformAttr defaultTransformOptions


defaultTransformOptions : TransformOptions
defaultTransformOptions =
    { rotationAxis =
        { x = 0
        , y = 0
        , z = 1
        }
    , origin = Center
    }


{-| -}
transformWith :
    TransformOptions
    -> (state -> Transform)
    -> Attribute state
transformWith =
    TransformAttr


{-| -}
scale : Float -> Transform
scale movement =
    Transform
        { x = at 0
        , y = at 0
        , z = at 0
        , facing =
            { x = at 0
            , y = at 0
            , z = at 0
            }
        , rotate = at 0
        , scaleX = at movement
        , scaleY = at movement
        , scaleZ = at 1
        }


{-| -}
scaleXY : Float -> Float -> Transform
scaleXY x y =
    Transform
        { x = at 0
        , y = at 0
        , z = at 0
        , facing =
            { x = at 0
            , y = at 0
            , z = at 0
            }
        , rotate = at 0
        , scaleX = at x
        , scaleY = at y
        , scaleZ = at 1
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
        , facing =
            { x = at 0
            , y = at 0
            , z = at 1
            }
        , rotate = at 0
        , scaleX = at 1
        , scaleY = at 1
        , scaleZ = at 1
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
        , facing =
            { x = at 0
            , y = at 0
            , z = at 1
            }
        , scaleX = at 1
        , scaleY = at 1
        , scaleZ = at 1
        }


{-| Rotate to a specific angle, where increasing numbers move clockwise.

The actual number provided should be in Elm standard angles (radians).

Alternatively you could use [turns or degrees](https://package.elm-lang.org/packages/elm/core/latest/Basics#degrees) instead.

-}
rotateTo : Float -> Transform
rotateTo angle =
    Transform
        { x = at 0
        , y = at 0
        , z = at 0
        , rotate = at angle
        , facing =
            { x = at 0
            , y = at 0
            , z = at 1
            }
        , scaleX = at 1
        , scaleY = at 1
        , scaleZ = at 1
        }


{-| Provide the duration it should take for one full rotation.
-}
rotating : Duration -> Transform
rotating dur =
    Transform
        { x = at 0
        , y = at 0
        , z = at 0
        , rotate = Animator.loop dur (wrap 0 (2 * pi))
        , facing =
            { x = at 0
            , y = at 0
            , z = at 1
            }
        , scaleX = at 1
        , scaleY = at 1
        , scaleZ = at 1
        }


{-| Have this element "look at" a specifc point.

The coordinates provided are relative to this element.

The default, which is where the element is looking directly outwards from the screen, is:

    Animator.Css.lookAt
        { x = 0
        , y = 0
        , z = 1
        }

-}
lookAt :
    { x : Float
    , y : Float
    , z : Float
    }
    -> Transform
    -> Transform
lookAt coords (Transform trans) =
    Transform
        { trans
            | facing =
                { x = at coords.x
                , y = at coords.y
                , z = at coords.z
                }
        }


{-| -}
type Period
    = Loop Duration
    | Repeat Int Duration


{-| -}
once : Duration -> Period
once =
    Repeat 1


{-| -}
loop : Duration -> Period
loop =
    Loop


{-| -}
repeat : Int -> Duration -> Period
repeat =
    Repeat


{-| -}
resting : Float -> Oscillator
resting fl =
    Timeline.Resting fl


{-| -}
in2d :
    Period
    ->
        { x : Oscillator
        , y : Oscillator
        , rotate : Oscillator
        , scaleX : Oscillator
        , scaleY : Oscillator
        }
    -> Transform
in2d period config =
    Transform
        { x = renderOsc period config.x
        , y = renderOsc period config.y
        , z = at 0
        , rotate = renderOsc period config.rotate
        , facing =
            { x = at 0
            , y = at 0
            , z = at 1
            }
        , scaleX = renderOsc period config.scaleX
        , scaleY = renderOsc period config.scaleY
        , scaleZ = at 1
        }


{-| -}
in3d :
    Period
    ->
        { x : Oscillator
        , y : Oscillator
        , z : Oscillator
        , rotate : Oscillator
        , scaleX : Oscillator
        , scaleY : Oscillator
        , scaleZ : Oscillator
        }
    -> Transform
in3d period config =
    Transform
        { x = renderOsc period config.x
        , y = renderOsc period config.y
        , z = renderOsc period config.z
        , rotate = renderOsc period config.rotate
        , facing =
            { x = at 0
            , y = at 0
            , z = at 1
            }
        , scaleX = renderOsc period config.scaleX
        , scaleY = renderOsc period config.scaleY
        , scaleZ = renderOsc period config.scaleZ
        }


renderOsc per oscillator =
    case oscillator of
        Timeline.Resting f ->
            at f

        Timeline.Oscillator pauses fn ->
            let
                activeDuration =
                    case per of
                        Repeat _ dur ->
                            dur

                        Loop dur ->
                            dur

                ( preparedFn, totalDuration ) =
                    Timeline.prepareOscillator activeDuration pauses fn
            in
            Interpolate.Oscillate
                Interpolate.FullDefault
                (case per of
                    Repeat i _ ->
                        Timeline.Repeat i totalDuration

                    Loop _ ->
                        Timeline.Loop totalDuration
                )
                preparedFn
