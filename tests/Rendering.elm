module Rendering exposing (suite)

import Animator
import Animator.Timeline as Timeline
import Animator.Transition as Transition
import Color
import Dict
import Expect
import Set
import Support.Css as Css
import Test exposing (Test, describe, test)
import Time


type alias Renderer =
    Timeline.Timeline (List Animator.Attribute) -> Animator.Css


suite : Test
suite =
    describe "Public timeline rendering contracts"
        (List.map
            (\( name, render ) -> describe name (initialValues render ++ transitions render))
            [ ( "onTimeline", \tl -> Animator.onTimeline tl identity |> Animator.toCss )
            , ( "onTimelineWith without steps", \tl -> Animator.onTimelineWith tl (\attrs -> ( attrs, [] )) |> Animator.toCss )
            , ( "css without steps", \tl -> Animator.css tl (\attrs -> ( attrs, [] )) )
            ]
        )


initialValues : Renderer -> List Test
initialValues render =
    List.map
        (\( name, attrs, expected ) ->
            test ("Preserves initial " ++ name) <|
                \_ ->
                    let
                        css =
                            Timeline.init attrs |> render
                    in
                    Expect.all
                        [ \_ -> Expect.equal "" css.keyframes
                        , \_ ->
                            Expect.equal
                                (List.map (\( key, value ) -> ( key, Just value )) expected)
                                (List.map (\( key, _ ) -> ( key, Css.property key css )) expected)
                        , \_ ->
                            -- Duplicate declarations hide lost vector channels.
                            Expect.equal
                                (List.length css.props)
                                (css.props |> List.map Tuple.first |> Set.fromList |> Set.size)
                        ]
                        ()
        )
        [ ( "opacity", [ Animator.opacity 0.35 ], [ ( "opacity", "0.35" ) ] )
        , ( "custom properties", [ Animator.px "width" 73, Animator.float "flex-grow" 2.5 ], [ ( "width", "73px" ), ( "flex-grow", "2.5" ) ] )
        , ( "color", [ Animator.color "background-color" Color.red ], [ ( "background-color", Color.toCssString Color.red ) ] )
        , ( "XYZ translation", [ Animator.x 12, Animator.y 34, Animator.z 56 ], [ ( "translate", "12px 34px 56px" ) ] )
        , ( "scale", [ Animator.scale 2 ], [ ( "scale", "2 2 2" ) ] )
        , ( "independent scale axes", [ Animator.scaleX 2, Animator.scaleY 3, Animator.scaleZ 4 ], [ ( "scale", "2 3 4" ) ] )
        , ( "rotation in turns", [ Animator.rotation 0.25 ], [ ( "rotate", "0 0 1 0.25turn" ) ] )
        , ( "translation alongside scale", [ Animator.x 12, Animator.y 34, Animator.scale 2 ], [ ( "translate", "12px 34px 0px" ), ( "scale", "2 2 2" ) ] )
        ]


transitions : Renderer -> List Test
transitions render =
    [ test "An unchanged property does not create an empty animation-list entry" <|
        \_ ->
            transition [ Animator.opacity 0, Animator.px "width" 73 ] [ Animator.opacity 1, Animator.px "width" 73 ]
                |> render
                |> (\css ->
                        case Css.property "animation" css of
                            Nothing ->
                                Expect.fail "Expected the opacity animation"

                            Just animation ->
                                animation
                                    |> String.split ","
                                    |> List.any (String.trim >> String.isEmpty)
                                    |> Expect.equal False
                   )
    , test "Translation and scale both reach their destinations" <|
        \_ ->
            transition
                [ Animator.x 0, Animator.y 0, Animator.z 0, Animator.scale 1 ]
                [ Animator.x 100, Animator.y 200, Animator.z 300, Animator.scale 2 ]
                |> render
                |> expectFinalProperties [ ( "translate", "100px 200px 300px" ), ( "scale", "2 2 2" ) ]
    , test "Multiple custom properties introduced after initialization are retained" <|
        \_ ->
            transition [] [ Animator.px "width" 100, Animator.px "height" 200 ]
                |> render
                |> expectFinalProperties [ ( "width", "100px" ), ( "height", "200px" ) ]
    , test "An explicit linear transition is included in its keyframes" <|
        \_ ->
            transition [ Animator.opacity 0 ] [ Animator.opacity 1 |> Animator.withTransition Transition.linear ]
                |> render
                |> (\css ->
                        case Css.keyframes css.keyframes of
                            Err error ->
                                Expect.fail error

                            Ok blocks ->
                                blocks
                                    |> List.concatMap .frames
                                    |> List.filterMap (.declarations >> Dict.get "animation-timing-function")
                                    |> List.any isLinear
                                    |> Expect.equal True
                   )
    , test "Ordinary clock ticks preserve CSS animation identity" <|
        \_ ->
            let
                tl =
                    transition [ Animator.opacity 0 ] [ Animator.opacity 1 ]
            in
            Expect.equal
                (render tl)
                (render (Timeline.update (Time.millisToPosix 10500) tl))
    ]


transition : List Animator.Attribute -> List Animator.Attribute -> Timeline.Timeline (List Animator.Attribute)
transition initial target =
    Timeline.init initial
        |> Timeline.to (Animator.ms 1000) target
        |> Timeline.update (Time.millisToPosix 10000)


expectFinalProperties : List ( String, String ) -> Animator.Css -> Expect.Expectation
expectFinalProperties expected css =
    case Css.keyframes css.keyframes of
        Err error ->
            Expect.fail error

        Ok blocks ->
            let
                finalProperties =
                    blocks
                        |> List.concatMap .frames
                        |> List.filter (\frame -> frame.percent == 100)
                        |> List.concatMap (.declarations >> Dict.toList)
            in
            Expect.equal
                (List.map (Tuple.mapSecond Just) expected)
                (List.map (\( name, _ ) -> ( name, Dict.get name (Dict.fromList finalProperties) )) expected)


isLinear : String -> Bool
isLinear easing =
    if easing == "linear" then
        True

    else if String.startsWith "cubic-bezier(" easing && String.endsWith ")" easing then
        case easing |> String.dropLeft 13 |> String.dropRight 1 |> String.split "," |> List.map (String.trim >> String.toFloat) of
            [ Just x1, Just y1, Just x2, Just y2 ] ->
                x1 == y1 && x2 == y2

            _ ->
                False

    else
        False
