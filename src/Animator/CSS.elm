module Animator.Css exposing
    ( animated
    , Attribute, opacity, fontColor, backgroundColor
    , with
    , y
    )

{-|


# CSS Animations

@docs div, animated

@docs Attribute, opacity, fontColor, backgroundColor

@docs with

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


type Attribute event
    = ColorAttribute String (event -> Color)
    | Attribute String (event -> Float) (Float -> String)
    | Movement String (event -> Movement) (Float -> String)



-- | Transform
--     { origin : (Origin)
--     , rotation : (Timeline state -> Timeline.Frames Float )
--     , scale : (Timeline state -> Timeline.Frames Float )
--     , x : (Timeline state -> Timeline.Frames Float )
--     , y : (Timeline state -> Timeline.Frames Float )
--     , z : (Timeline state -> Timeline.Frames Float )
--     }


type Origin
    = Origin


type Animated event msg
    = Animated String (List (Attribute event)) (List (Html.Attribute msg)) (List (Html msg))



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
renderFrame i total name frames renderer rendered =
    case frames of
        [] ->
            rendered

        frm :: remain ->
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
                        String.fromFloat (100 * (toFloat i / toFloat (total - 1))) ++ "% {" ++ name ++ ": " ++ renderer frm ++ ";}\n"
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
    in
    Html.Keyed.node "div"
        attrs
        [ ( "animator-stylesheet", stylesheet (renderAnimations animations) )
        , ( "animated-node"
          , Html.div (Attr.class (renderClassName "" animations) :: attrs) children
          )
        ]


{-| -}
opacity : (event -> Float) -> Attribute event
opacity lookup =
    Attribute
        "opacity"
        lookup
        String.fromFloat


{-| -}
y : (event -> Movement) -> Attribute event
y lookup =
    Movement "transform" lookup (\f -> "translateY(" ++ String.fromFloat f ++ "px)")


{-| -}
backgroundColor : (event -> Color.Color) -> Attribute event
backgroundColor lookup =
    ColorAttribute "background-color" lookup


{-| -}
fontColor : (event -> Color.Color) -> Attribute event
fontColor lookup =
    ColorAttribute "color" lookup



{- ANIMATOR -}


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
