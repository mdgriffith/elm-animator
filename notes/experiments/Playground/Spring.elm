module Playgroun.Spring exposing (main)

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Svg
import Svg.Attributes as SvgA
import Internal.Spring as Spring
import Duration
import Internal.Interpolate as Interpolate
import Internal.Bezier as Bezier
import Internal.Css as Css


{- Welcome to the Spring Playground!

   The main goal is to efficiently render a spring system into a List CubicBezier

   We have the constraint of always working on underdamped springs (meaning they will always settle).


   A secondary goal(though this one isnt nearly as critical as the first), is can we calculate the position and velocity of a spring without having to use fold?



   Basically, can we implement this function:

       stepOver :
           Milliseconds
           -> SpringParams
           -> Float
           ->
               { velocity : Float
               , position : Float
               }
           ->
               { velocity : Float
               , position : Float
               }

   without having to fold over a bunch of tiny steps to find the result

-}


{-| Create a basic spring that is half-wobbly, and settles in 1000ms
-}
basic : Spring.SpringParams
basic =
    -- Spring.select 0.5 (Duration.milliseconds 1000)
    Spring.select 0.5 (Duration.milliseconds 1000)
        -- |> Debug.log "params"
        

full : Spring.SpringParams
full =
    -- Spring.select 0.5 (Duration.milliseconds 1000)
    Spring.select 1 (Duration.milliseconds 1000)
        -- |> Debug.log "params"


null : Spring.SpringParams
null =
    -- Spring.select 0.5 (Duration.milliseconds 1000)
    Spring.select 0 (Duration.milliseconds 1000)
        -- |> Debug.log "params"
       



main : Html msg
main =
    div []
        [ h1 [] [ text "Spring Playground" ]
        , Svg.svg
            [ SvgA.width "2000px"
            , SvgA.height "2000px"
            , SvgA.viewBox "0 -100 1000 1400"
            , SvgA.style "border: 4px dashed #eee;"
            ]
            [ 
                 viewSpring basic
            , viewZeros basic
            --  viewSpring full
            -- , viewZeros full
            -- , viewSpring null
            -- , viewZeros null
            -- , viewBezier 
            --     (Spline 
                
                
            --     )
            , Svg.line
                [ SvgA.x1 "0"
                , SvgA.y1 "0"
                , SvgA.x2 "1000"
                , SvgA.y2 "0"
                , SvgA.stroke "black"
                , SvgA.strokeWidth "3"
                ]
                []
            ]
        ]


viewSpringBezier spring =
    let
        ps =
            Spring.peaks spring
                (toFloat 0)
                1000
                { velocity = 0
                , position = 0
                }

        pairs =
            List.map2 Tuple.pair 
                ps
                (List.drop 1 ps)

        
    in
   Svg.g [] 
            (List.map 
                (\(one, two) -> 
                     let
                        posOne =
                            Spring.analytical spring
                                (Duration.milliseconds one)
                                1000
                                { velocity = 0
                                , position = 0
                                }
                                
                        
                        posTwo =
                            Spring.analytical spring
                                (Duration.milliseconds two)
                                1000
                                { velocity = 0
                                , position = 0
                                }
                               
                        offsetOne =
                            (two - one) 
                                * ((0.25) )

                        ctrlOne =
                             Point (round ((one) + offsetOne)) (round (1000 - posOne.position))


                        offsetTwo =
                            (two - one) 
                               
                                * ((0.55 ))
                        
                        ctrlTwo =
                            Point (round ((two) - offsetTwo)) (round (1000 - posTwo.position))
                            
                           

                    in
                    Svg.g [] 
                        [ viewBezier
                            { one = 
                                Point (round one) (round (1000 - posOne.position))
                            , oneControl =
                                ctrlOne
                            , two =
                                Point (round two) (round (1000 - posTwo.position))
                            , twoControl =
                                ctrlTwo
                            }
                       
                        
                        ]
                )
                (pairs)
            )
        


viewZeros spring =
    let
        ps =
            Spring.peaks spring
                (toFloat 0)
                1000
                { velocity = 0
                , position = 0
                }

        pairs =
            List.map2 Tuple.pair 
                ps
                (List.drop 1 ps)

        
    in
    Svg.g [] 
        [ Svg.g []
            (List.map
                (\z ->
                    let
                        new =
                            Spring.analytical spring
                                (Duration.milliseconds z)
                                1000
                                { velocity = 0
                                , position = 0
                                }

                    in
                    Svg.line
                            [ SvgA.x1 (String.fromFloat z)
                            , SvgA.y1 "0"
                            , SvgA.x2 (String.fromFloat z)
                            , SvgA.y2 "1000"
                            , SvgA.stroke "black"
                            , SvgA.strokeWidth "3"
                            ]
                            []
                        
                        
                        
                )
                ( ps)
            )
        , Svg.g [] 
            (List.map 
                (\(one, two) -> 
                     let
                        posOne =
                            Spring.analytical spring
                                (Duration.milliseconds one)
                                1000
                                { velocity = 0
                                , position = 0
                                }
                                
                        
                        posTwo =
                            Spring.analytical spring
                                (Duration.milliseconds two)
                                1000
                                { velocity = 0
                                , position = 0
                                }
                               

                        -- factor = 0.05

                        -- spread = 0.1
                        factor = 0

                        spread = 0

                        offsetOne =
                            (two - one) 
                                -- * 0.257
                                * ((0.33 - factor) - spread)

                        ctrlOne =
                            -- + 90
                             Point (round ((one) + offsetOne)) (round (1000 - posOne.position))


                        offsetTwo =
                            (two - one) 
                                -- * 0.55182845698119
                                * ((0.55 + factor) - spread)
                        
                        ctrlTwo =
                            -- - 197
                            Point (round ((two) - offsetTwo)) (round (1000 - posTwo.position))
                            
                            -- 140

                            -- 355

                            -- 140 / 355 -> 0.39

                            -- (355 - 140) / 355 -> 0.605

                        bez =
                            { one = 
                                Point (round one) (round (1000 - posOne.position))
                            , oneControl =
                                ctrlOne
                            , two =
                                Point (round two) (round (1000 - posTwo.position))
                            , twoControl =
                               ctrlTwo
                            }

                        spline =
                            Bezier.Spline 
                                ( Bezier.Point ( one) ( (1000 - posOne.position)))
                                 { x = toFloat ctrlOne.x
                                ,  y = toFloat ctrlOne.y

                                }
                                { x = toFloat ctrlTwo.x
                                ,  y = toFloat ctrlTwo.y

                                }
                                ( Bezier.Point ( two) ( (1000 - posTwo.position)))

                        mid = 
                            round (one + (0.5 * (two - one)))

                    in
                    Svg.g [] 
                        [ viewBezier bez
                        , dot (Debug.log "point" (Bezier.pointOn spline 0.5))
                        , line 
                            { x = mid
                            , y = 0

                            }
                            { x = mid
                            , y = 1000

                            }
                       
                        
                        ]
                )
                (pairs)
            )
        ]
        

    


viewSpring spring =
    Svg.g []
        (List.range 0 100
            |> List.map ((*) 10)
            |> List.map
                (\t ->
                    let
                        old =
                            Spring.stepOver  (Duration.milliseconds (toFloat t))
                                spring
                                1000
                                { velocity = 0
                                , position = 0
                                }

                        new =
                            Spring.analytical spring
                                (Duration.milliseconds (toFloat t))
                                1000
                                { velocity = 0
                                , position = 0
                                }
                        _ = Debug.log "spring" (t, new)
                    in
                    Svg.g []
                        [ Svg.circle
                            [ SvgA.cx (String.fromInt t)
                            , SvgA.cy
                                ((1000 - new.position)
                                    |> String.fromFloat
                                )
                            , SvgA.r "12"
                            , SvgA.fill "red"
                            , SvgA.opacity "0.5"
                            ]
                            []
                        , Svg.circle
                            [ SvgA.cx (String.fromInt t)
                            , SvgA.cy
                                (old.position
                                    |> String.fromFloat
                                )
                            , SvgA.r "3"
                            ]
                            []
                        , Svg.circle
                            [ SvgA.cx (String.fromInt t)
                            , SvgA.cy
                                ((new.velocity / 10)
                                    |> String.fromFloat
                                )
                            , SvgA.r "20"
                            , SvgA.fill "blue"
                            , SvgA.opacity "0.2"
                            ]
                            []
                        , Svg.circle
                            [ SvgA.cx (String.fromInt t)
                            , SvgA.cy
                                (old.velocity
                                    / 10
                                    |> String.fromFloat
                                )
                            , SvgA.r "10"
                            , SvgA.fill "green"
                            ]
                            []
                        ]
                )
        )






type alias Bezier =
    { one : Point
    , oneControl : Point
    , two : Point
    , twoControl : Point
    }


type alias Point =
    { x : Int, y : Int }



viewBezier : Bezier -> Html msg
viewBezier bez =
    Svg.g [] 
        [ Svg.path
            [ SvgA.d 
                -- ("M 10 10 C 20 20, 40 20, 50 10")
                (String.join " "
                    [ "M "
                    ++ renderPoint bez.one
                    ++ " C "
                        ++ renderPoint bez.oneControl
                        ++ ", "
                        ++ renderPoint bez.twoControl
                        ++ ", "
                        ++ renderPoint bez.two
                    ]

                )
            , SvgA.stroke "black"
            , SvgA.strokeWidth "5"
            , SvgA.fill "rgba(0,0,0,0)"
            ]
            []

         ,  Svg.circle
            [ SvgA.cx (String.fromInt bez.one.x)
            , SvgA.cy (String.fromInt bez.one.y)
            , SvgA.r "25"
            , SvgA.fill "black"
            
            ]
            []

        ,  Svg.circle
            [ SvgA.cx (String.fromInt bez.two.x)
            , SvgA.cy (String.fromInt bez.two.y)
            , SvgA.r "25"
            , SvgA.fill "black"
            
            ]
            []

        ,  Svg.circle
            [ SvgA.cx (String.fromInt bez.oneControl.x)
            , SvgA.cy (String.fromInt bez.oneControl.y)
            , SvgA.r "15"
            , SvgA.fill "red"
            ]
            []
        ,  Svg.circle
            [ SvgA.cx (String.fromInt bez.twoControl.x)
            , SvgA.cy (String.fromInt bez.twoControl.y)
            , SvgA.r "15"
            , SvgA.fill "red"
            
            ]
            []
        , line bez.one bez.oneControl
        , line bez.two bez.twoControl
        ]


dot point =
    Svg.circle
        [ SvgA.cx (String.fromFloat point.x)
        , SvgA.cy
            (point.y
                |> String.fromFloat
            )
        , SvgA.r "24"
        , SvgA.fill "red"
        ]
        []

line one two =
    Svg.line
    [ SvgA.x1 (String.fromInt one.x)
    , SvgA.y1 (String.fromInt one.y)
    , SvgA.x2 (String.fromInt two.x)
    , SvgA.y2 (String.fromInt two.y)
    , SvgA.stroke "black"
    , SvgA.strokeWidth "3"
    ]
    []


viewPath : List Bezier -> Html msg
viewPath segments =
    Svg.path
        [ SvgA.d (renderBezierString segments "M 0,0")
        , SvgA.stroke "black"
        , SvgA.strokeWidth "5"
        , SvgA.fill "rgba(0,0,0,0)"
        ]
        []


renderBezierString segments str =
    case segments of
        [] ->
            str

        segment :: remaining ->
            renderBezierString remaining
                (str
                    ++ " C "
                    ++ renderPoint segment.oneControl
                    ++ " "
                    ++ renderPoint segment.twoControl
                    ++ " "
                    ++ renderPoint segment.two
                )


renderPoint : Point -> String
renderPoint p =
    String.fromInt p.x ++ " " ++ String.fromInt p.y



{- SPRING INTERPOLATION from ELM ANIMATOR -}
{- I kept the comments in here though they might be overkill.

   Main insights:

      - We only deal with underdamped springs.
      - We do this by only defining springs by "wobbliness and duration".
      - Wobble is essentally a damping ratio, but clamped to a certain range of values.
      - Settling time of a spring can be pretty easily be calculated for a spring if you know it's underdamped.
      - Stiffness and mass are constants here as they don't meaningfully affect the personality of the spring's movement.

-}


