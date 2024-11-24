module InternalAnim.Render exposing (Key, Keyframes, keyframes, toInitialProp)

{-| -}

import Bezier
import Bezier.Spring as Spring
import Color
import Dict exposing (Dict)
import InternalAnim.Css as Css
import InternalAnim.Css.Props as Props
import InternalAnim.Duration as Duration
import InternalAnim.Move as Move
import InternalAnim.Quantity as Quantity
import InternalAnim.Render.Css as ToString
import InternalAnim.Time as Time
import InternalAnim.Timeline as Timeline
import InternalAnim.Transition as Transition
import InternalAnim.Units as Units
import Set exposing (Set)
import Time


{-| -}
type alias Keyframes =
    { animationName : String
    , animationProp : String
    , keyframes : String
    }


type alias Id =
    Int


type Key
    = TranslateKey
    | ScaleKey
    | ColorKey String
      --      id  name   current initialState
    | PropKey Int String Motion Props.Format Float


toInitialProp : Key -> ( String, String )
toInitialProp key =
    case key of
        TranslateKey ->
            ( "translate", "0% 0" )

        ScaleKey ->
            ( "scale", "1" )

        ColorKey name ->
            ( name, "rgba(0,0,0,0)" )

        PropKey _ name _ format value ->
            ( name, Props.format format value )


{--}
type alias Motion =
    { position : Float -- pixels
    , velocity : Float -- pixels per second
    }


keyframes :
    Timeline.Timeline state
    -> (state -> List Css.Prop)
    -> List ( Key, List Keyframes )
keyframes timeline lookup =
    let
        present =
            getInitial timeline lookup
    in
    Timeline.foldpAll (Timeline.getUpdatedAt timeline)
        lookup
        (\_ -> List.map (\key -> ( key, [] )) present)
        toKeyframes
        timeline


getInitial : Timeline.Timeline event -> (event -> List Css.Prop) -> List Key
getInitial timeline lookup =
    let
        presence =
            Timeline.foldpAll (Timeline.getUpdatedAt timeline)
                lookup
                (\props ->
                    List.foldl
                        toInitialProps
                        { props = Dict.empty
                        , translate = False
                        , scale = False
                        }
                        props
                )
                (\get target _ _ _ _ existing ->
                    let
                        props =
                            get (Timeline.getEvent target)
                    in
                    List.foldl
                        addInitialProp
                        existing
                        props
                )
                timeline
    in
    Dict.values presence.props


appendIf : Bool -> Key -> List Key -> List Key
appendIf condition key keys =
    if condition then
        key :: keys

    else
        keys


type alias PropPresence =
    { props : Dict String Key
    , translate : Bool
    , scale : Bool
    }


toInitialProps : Css.Prop -> PropPresence -> PropPresence
toInitialProps prop rendered =
    case prop of
        Css.Prop id name movement format ->
            if Props.isTranslateId id then
                { props = Dict.insert name TranslateKey rendered.props
                , translate = True
                , scale = rendered.scale
                }

            else if Props.isScaleId id then
                { props = Dict.insert name ScaleKey rendered.props
                , translate = rendered.translate
                , scale = True
                }

            else
                { props =
                    Dict.insert name
                        (PropKey id
                            name
                            { position = Move.toValue movement
                            , velocity = 0
                            }
                            format
                            (Move.toValue movement)
                        )
                        rendered.props
                , translate = rendered.translate
                , scale = rendered.scale
                }

        Css.ColorProp name _ ->
            { props = Dict.insert name (ColorKey name) rendered.props
            , translate = rendered.translate
            , scale = rendered.scale
            }


{-| If a props isn't defined in the first state, but is defined in the future, we want to add it.
-}
addInitialProp : Css.Prop -> PropPresence -> PropPresence
addInitialProp prop rendered =
    case prop of
        Css.Prop id name movement format ->
            if Props.isTranslateId id then
                if rendered.translate then
                    rendered

                else
                    { props = Dict.insert name TranslateKey rendered.props
                    , translate = True
                    , scale = rendered.scale
                    }

            else if Props.isScaleId id then
                if rendered.scale then
                    rendered

                else
                    { props = Dict.insert name ScaleKey rendered.props
                    , translate = rendered.translate
                    , scale = True
                    }

            else if Dict.member name rendered.props then
                rendered

            else
                { props =
                    Dict.insert name
                        (PropKey id
                            name
                            { position = Props.defaultPosition id
                            , velocity = 0
                            }
                            format
                            (Props.defaultPosition id)
                        )
                        rendered.props
                , translate = rendered.translate
                , scale = rendered.scale
                }

        Css.ColorProp name _ ->
            if Dict.member name rendered.props then
                rendered

            else
                { props = Dict.insert name (ColorKey name) rendered.props
                , translate = rendered.translate
                , scale = rendered.scale
                }


{-|

    Render an animation as a set of keyframe animations


    Keyframes can be rendered out of order, with multiple keyframes for the same percentage


    e.g.

    @keyframes anim {
        0% { translate: 0% 0; }
        100% { translate: 100% 0; }

        0%, 100% { scale: 1; }
        5%, 95% { scale: 1.2; }

        0% { rotate: 0deg; }
        10%, 90% { rotate: 180deg; }
        100% { rotate: 360deg; }
    }

        type Key
                = TranslateKey
                | ScaleKey
                | ColorKey String
                | PropKey String

-}
toKeyframes : Timeline.Transition state (List Css.Prop) (List ( Key, List Keyframes ))
toKeyframes lookup target now startTime endTime future renderedProps =
    let
        props =
            lookup (Timeline.getEvent target)

        duration =
            -- Time.duration startTime (Timeline.endTime target)
            Time.duration startTime (Timeline.startTime target)

        delay =
            Duration.seconds 0

        targetTime =
            Timeline.startTime target

        progress =
            Time.progress startTime targetTime now

        hasStartedOrUpcoming =
            Time.thisAfterOrEqualThat startTime now
    in
    List.map
        (\( key, keyFrameList ) ->
            case key of
                TranslateKey ->
                    ( key
                    , keyFrameList
                    )

                ScaleKey ->
                    ( key
                    , keyFrameList
                    )

                ColorKey name ->
                    case getColor name props of
                        Just (Move.Pos initialTransition targetColor _) ->
                            let
                                animName =
                                    name ++ "-" ++ Props.colorHash targetColor
                            in
                            ( key
                            , { animationName = animName
                              , animationProp = ToString.animation duration delay 1 animName
                              , keyframes =
                                    ToString.keyframes animName
                                        (ToString.frame 100
                                            (ToString.prop name (Color.toCssString targetColor))
                                        )
                              }
                                :: keyFrameList
                            )

                        Nothing ->
                            ( key
                            , keyFrameList
                            )

                PropKey targetId name startingValue format default ->
                    case getProp targetId name props of
                        Just prop ->
                            let
                                newAnims =
                                    if hasStartedOrUpcoming then
                                        movementToAnims now delay duration name format startingValue prop

                                    else
                                        []

                                newMotion =
                                    Transition.atX
                                        progress
                                        startTime
                                        targetTime
                                        (Move.toTransition prop)
                                        -- current
                                        { position = Units.pixels startingValue.position
                                        , velocity = Units.pixelsPerSecond startingValue.velocity
                                        }
                                        (Move.toValue prop)
                            in
                            ( PropKey targetId
                                name
                                { position = Units.inPixels newMotion.position
                                , velocity = Units.inPixelsPerSecond newMotion.velocity
                                }
                                format
                                default
                            , newAnims ++ keyFrameList
                            )

                        Nothing ->
                            ( key
                            , keyFrameList
                            )
        )
        renderedProps


hashInitialMovement name now transition format target =
    let
        nowMs =
            Time.inMilliseconds now

        adjustedTime =
            -- This is an arbitraray posix time that is in the past
            -- Because it's the time as I write this code.
            if nowMs > 1723547863409 then
                nowMs - 1723547863409

            else
                0
    in
    (name ++ "-")
        -- Arbitrary point that
        ++ (String.fromInt (round adjustedTime) ++ "-")
        ++ (Transition.hash transition ++ "-")
        ++ Props.hashFormat format target


movementToAnims : Time.Absolute -> Duration.Duration -> Duration.Duration -> String -> Props.Format -> Motion -> Move.Move Float -> List Keyframes
movementToAnims now delay duration name format startMotion target =
    let
        transition =
            Move.toTransition target

        targetValue =
            Move.toValue target
    in
    if startMotion.position == targetValue then
        -- Todo, it's possible that we could have a spring transition that ultimately doesn't move, but bobbles around a little bit.
        []

    else
        let
            animName =
                hashInitialMovement name now transition format targetValue

            dwellAnimations =
                Move.toDwellSequence target
                    |> List.foldl
                        (\seq gathered ->
                            sequenceToAnimation now name format seq :: gathered
                        )
                        []
        in
        { animationName = animName
        , animationProp = ToString.animation duration delay 1 animName
        , keyframes =
            ToString.keyframes animName
                (transitionToKeyframes duration
                    name
                    format
                    startMotion
                    transition
                    targetValue
                )
        }
            :: dwellAnimations


{-|

    -- From the example, where the target is 186
    -- The last value is off by 3 orders of magnitude :joy:
    0% {
        translate: 0px;
        animation-timing-function: cubic-bezier(0.17,0.17,0.7,0.7);
    }
    12% {
        translate: 112px;
        animation-timing-function: cubic-bezier(0.3,0.34,0.7,0.83);
    }
    25% {
        translate: 213px;
        animation-timing-function: cubic-bezier(0.3,-13.13,0.7,-3.36);
    }
    37% {
        translate: 213px;
        animation-timing-function: cubic-bezier(0.3,0.21,0.7,0.79);
    }
    50% {
        translate: 185px;
        animation-timing-function: cubic-bezier(0.3,0.64,0.7,0.92);
    }
    62% {
        translate: 178px;
        animation-timing-function: cubic-bezier(0.3,0.03,0.7,0.73);
    }
    75% {
        translate: 183px;
        animation-timing-function: cubic-bezier(0.3,0.39,0.7,0.84);
    }
    87% {
        translate: 186px;
        animation-timing-function: cubic-bezier(0.3,-0.86,0.7,0.46);
    }
    100% {
        translate: 185940px;
    }



    Move to third position

    0% {
        translate: 0px;
        animation-timing-function: cubic-bezier(0.17,0.17,0.7,0.7);
    }
    12% {
        translate: 195px;
        animation-timing-function: cubic-bezier(0.3,0.34,0.7,0.83);
    }
    25% {
        translate: 372px;
        animation-timing-function: cubic-bezier(0.3,-13.13,0.7,-3.36);
    }
    37% {
        translate: 370px;
        animation-timing-function: cubic-bezier(0.3,0.21,0.7,0.79);
    }
    50% {
        translate: 322px;
        animation-timing-function: cubic-bezier(0.3,0.64,0.7,0.92);
    }
    62% {
        translate: 309px;
        animation-timing-function: cubic-bezier(0.3,0.03,0.7,0.73);
    }
    75% {
        translate: 318px;
        animation-timing-function: cubic-bezier(0.3,0.39,0.7,0.84);
    }
    87% {
        translate: 324px;
        animation-timing-function: cubic-bezier(0.3,-0.86,0.7,0.46);
    }
    100% {
        translate: 323636px;
    }

    To 322



    Transition keyframes: { count = 8, finalValue = 322, splines = [Spline { x = 0, y = 185 } { x = 6.708333333333333, y = 187.13818069458338 } { x = 28.174999999999997, y = 191.81437474618713 } { x = 40.25, y = 197.82908416750035 },Spline { x = 40.25, y = 197.82908416750035 } { x = 52.325, y = 204.88366791132245 } { x = 68.425, y = 218.08997800402824 } { x = 80.5, y = 227.90956049517226 },Spline { x = 80.5, y = 227.90956049517226 } { x = 92.575, y = 238.09884060309136 } { x = 108.675, y = 253.40633901166706 } { x = 120.75, y = 263.99313961259804 },Spline { x = 120.75, y = 263.99313961259804 } { x = 132.825, y = 274.4475340306158 } { x = 148.925, y = 288.450794380649 } { x = 161, y = 297.68777673722093 },Spline { x = 161, y = 297.68777673722093 } { x = 173.075, y = 306.4877348888135 } { x = 189.175, y = 317.1917511835213 } { x = 201.25, y = 323.93291273127306 },Spline { x = 201.25, y = 323.93291273127306 } { x = 213.325, y = 330.11459614721747 } { x = 229.425, y = 336.79379744254396 } { x = 241.5, y = 340.727336283524 },Spline { x = 241.5, y = 340.727336283524 } { x = 253.575, y = 344.11926197208413 } { x = 269.675, y = 347.00357423321395 } { x = 281.75, y = 348.416743334021 },Spline { x = 281.75, y = 348.416743334021 } { x = 293.825, y = 349.3946387719228 } { x = 309.925, y = 349.29752500078473 } { x = 322, y = 348.8184148541855 }], startPos = 185 }


    percent: { percent = 0, spline = Spline { x = 0, y = 185 } { x = 6.708333333333333, y = 187.13818069458338 } { x = 28.174999999999997, y = 191.81437474618713 } { x = 40.25, y = 197.82908416750035 }, val = 185 }
    percent: { percent = 4.025, spline = Spline { x = 40.25, y = 197.82908416750035 } { x = 52.325, y = 204.88366791132245 } { x = 68.425, y = 218.08997800402824 } { x = 80.5, y = 227.90956049517226 }, val = 197.82908416750035 }
    percent: { percent = 8.05, spline = Spline { x = 80.5, y = 227.90956049517226 } { x = 92.575, y = 238.09884060309136 } { x = 108.675, y = 253.40633901166706 } { x = 120.75, y = 263.99313961259804 }, val = 227.90956049517226 }
    percent: { percent = 12.075, spline = Spline { x = 120.75, y = 263.99313961259804 } { x = 132.825, y = 274.4475340306158 } { x = 148.925, y = 288.450794380649 } { x = 161, y = 297.68777673722093 }, val = 263.99313961259804 }
    percent: { percent = 16.1, spline = Spline { x = 161, y = 297.68777673722093 } { x = 173.075, y = 306.4877348888135 } { x = 189.175, y = 317.1917511835213 } { x = 201.25, y = 323.93291273127306 }, val = 297.68777673722093 }
    percent: { percent = 20.125, spline = Spline { x = 201.25, y = 323.93291273127306 } { x = 213.325, y = 330.11459614721747 } { x = 229.425, y = 336.79379744254396 } { x = 241.5, y = 340.727336283524 }, val = 323.93291273127306 }
    percent: { percent = 24.15, spline = Spline { x = 241.5, y = 340.727336283524 } { x = 253.575, y = 344.11926197208413 } { x = 269.675, y = 347.00357423321395 } { x = 281.75, y = 348.416743334021 }, val = 340.727336283524 }
    percent: { percent = 28.175, spline = Spline { x = 281.75, y = 348.416743334021 } { x = 293.825, y = 349.3946387719228 } { x = 309.925, y = 349.29752500078473 } { x = 322, y = 348.8184148541855 }, val = 348.416743334021 }

-}
transitionToKeyframes : Duration.Duration -> String -> Props.Format -> Motion -> Transition.Transition -> Float -> String
transitionToKeyframes duration name format startMotion transition finalValue =
    case transition of
        Transition.Transition spline ->
            ToString.frame 0 (ToString.timingFunction spline)
                ++ ToString.frame 100
                    (ToString.prop name (Props.format format finalValue))

        Transition.Wobble wobble ->
            let
                durationMs =
                    Duration.inMilliseconds duration

                startPos =
                    startMotion.position

                splines =
                    Spring.segments
                        -- Select a spring that will wobble and settle in 1000 milliseconds
                        (Spring.new
                            { wobble = wobble.wobble
                            , quickness = wobble.quickness
                            , settleMax = durationMs
                            }
                        )
                        { position = startPos

                        -- intro velocity
                        , velocity =
                            startMotion.velocity

                        -- wobble.introVelocity
                        }
                        finalValue

                -- _ =
                --     Debug.log "Transition keyframes"
                --         { count = List.length splines -- should be roughly 8
                --         , duratino = durationMs
                --         , splines = splines
                --         , startPos = startPos
                --         , finalValue = finalValue
                --         }
                allFrames =
                    List.foldl
                        (\spline rendered ->
                            let
                                -- _ =
                                --     let
                                --         _ =
                                --             Debug.log "  > " ( startPos, finalValue )
                                --     in
                                --     Debug.log "x" ( Bezier.first spline |> .x, durationMs, value )
                                percent =
                                    (Bezier.first spline |> .x) / durationMs

                                value =
                                    Bezier.first spline |> .y

                                normalizedSpline =
                                    Bezier.normalize spline
                            in
                            rendered
                                ++ ToString.frame (roundPercent percent)
                                    (ToString.timingFunction spline
                                        ++ ToString.prop name (Props.format format value)
                                    )
                        )
                        ""
                        splines
            in
            allFrames
                ++ ToString.frame 100
                    (ToString.prop name (Props.format format finalValue))


sequenceToAnimation : Time.Absolute -> String -> Props.Format -> Move.Sequence Float -> Keyframes
sequenceToAnimation now name format ((Move.Sequence count delay duration steps) as seq) =
    let
        animationName =
            Move.hash now
                name
                seq
                (Props.hashFormat format)
    in
    { animationName = animationName
    , animationProp = ToString.animation duration delay count animationName
    , keyframes =
        ToString.keyframes animationName
            (List.foldl
                (stepToKeyframe name format duration)
                ( Duration.seconds 0, "" )
                steps
                |> Tuple.second
            )
    }


stepToKeyframe : String -> Props.Format -> Duration.Duration -> Move.Step Float -> ( Duration.Duration, String ) -> ( Duration.Duration, String )
stepToKeyframe name format totalDuration (Move.Step stepDuration transition value) ( lastDuration, lastKeyframe ) =
    let
        newDuration =
            Quantity.plus lastDuration stepDuration

        percent =
            if Duration.isZero totalDuration then
                0

            else
                Duration.inSeconds lastDuration / Duration.inSeconds totalDuration
    in
    ( newDuration
    , lastKeyframe
        ++ ToString.frame (roundPercent percent) (ToString.prop name (Props.format format value))
    )


roundPercent : Float -> Int
roundPercent percent =
    if percent < 0.01 then
        0

    else if percent > 0.98 then
        100

    else
        round (percent * 100)


{-| -}
getColor : String -> List Css.Prop -> Maybe (Move.Move Color.Color)
getColor targetName props =
    case props of
        [] ->
            Nothing

        (Css.Prop _ _ _ _) :: remain ->
            getColor targetName remain

        (Css.ColorProp name movement) :: remain ->
            if targetName == name then
                Just movement

            else
                getColor targetName remain


{-| -}
getProp : Id -> String -> List Css.Prop -> Maybe (Move.Move Float)
getProp targetId targetName props =
    case props of
        [] ->
            Nothing

        (Css.Prop id name move format) :: remain ->
            if (targetId - Props.noId) == 0 then
                if name == targetName then
                    Just move

                else
                    getProp targetId targetName remain

            else if id == targetId then
                Just move

            else
                getProp targetId targetName remain

        (Css.ColorProp _ _) :: remain ->
            getProp targetId targetName remain
