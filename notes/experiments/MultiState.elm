module MultiState exposing (main)

{-| Animated page transitions!

This example is meant to show a few things.

    1.  That page transitions are just like animating any other state, we'll just create an `Animator.Timeline Page` and animate with that.
    2.  How to use CSS keyframes by using the `Animator.Css` module
    3.  How to handle routing so that the url changes as your transition.

-}

import Animator
import Animator.Timeline as Timeline
import Animator.Watcher as Watcher
import Browser
import Browser.Events
import Browser.Navigation
import Color
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Playground.View.Bezier
import Internal.Timeline
import Internal.Css
import Time
import Animator.Value
import Animator.Watcher

{-| -}
type alias Model =
    { ball : Timeline.Timeline Int
    , queue : Bool
    }


positions =
    { all = [ 0, 1, 2, 3 ]
    , start = 0
    , end = 3
    }


main =
    Browser.element
        { init =
            \() ->
                ( { ball = 
                    Timeline.init positions.start
                        |> Timeline.scale 0.5
                  , queue = False
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ animator
                        |> Watcher.toSubscription Tick model
                    ]
        }




animator : Watcher.Watching Model
animator =
    Watcher.init
        -- *NOTE*  We're using `the Animator.watching` instead of `Animator.watching`.
        -- Instead of asking for a constant stream of animation frames, it'll only ask for one
        -- and we'll render the entire css animation in that frame.
        |> Watcher.watching .ball
            (\newBall model ->
                { model | ball = newBall }
            )



{- UPDATING -}


type Msg
    = Tick Time.Posix
    | GoTo Int
    | ToggleQueue


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | ball = Timeline.update newTime model.ball }
            , Cmd.none
            )

        GoTo target ->
            ( { model
                | ball =
                    if model.queue then
                        let
                            current =
                                Timeline.current model.ball
                                    
                            range =
                                if target > current then
                                    List.range current target
                                        |> List.drop 1

                                else
                                    List.range target current
                                        |> List.reverse
                                        |> List.drop 1

                            --correctRange =
                            --    if highest - target == 0 then
                            --        --List.drop 1
                            --        range
                            --
                            --    else
                            --        --List.drop 1
                            --        List.reverse range
                        in
                        model.ball
                            |> Timeline.queue
                                (range
                                    |> Debug.log "QUEUING RANGE"
                                    |> List.map
                                        (\i ->
                                            Timeline.transitionTo (Timeline.ms 1000) i
                                        )
                                )

                    else
                        model.ball
                            |> Timeline.to (Timeline.ms 1000) target
              }
            , Cmd.none
            )

        ToggleQueue ->
            ( { model
                | queue =
                    not model.queue
              }
            , Cmd.none
            )



{- Actually viewing our pages! -}


view : Model -> Html Msg
view model =
    let
        _ =
            Debug.log "------------VIEWING2" "----------"
    in
    div []
        [ stylesheet
        , div
            [ Attr.class "root"
            ]
            [ div
                [ Attr.class "page-row"
                ]
                [ viewBall model.ball
                ]
            , div
                [ Attr.class "page-row"
                ]
                [ viewBallTarget 0
                , viewBallTarget 1
                , viewBallTarget 2
                , viewBallTarget 3
                , viewBallTarget 4
                , viewBallTarget 5
                , viewBallTarget 6
                ]
            , div
                [ Events.onClick ToggleQueue ]
                [ if model.queue then
                    text "queue"

                  else
                    text "direct"
                ]

            -- , Playground.View.Bezier.view 
            --     [ { startedAt = Internal.Timeline.getCurrentTime model.ball
            --       , props = Internal.Css.propsToRenderedProps model.ball renderProps

            --     }

            --     ]

            ]
        ]


viewBallTarget index =
    div
        [ Attr.class "ball-target"
        , Attr.style "transform" ("translateX(" ++ String.fromFloat (toFloat index * 200) ++ "px)")
        , Attr.style "cursor" "pointer"
        , Events.onClick (GoTo index)
        ]
        []


red = Color.rgb 1 0 0

blue = Color.rgb 0 0 1

viewBall timeline =
    Animator.div 
        (Animator.onTimeline timeline
            renderProps
        )
        -- (Timeline.current timeline 
        --     |> renderProps
        --     |> Animator.transition (Animator.ms 1000)

        -- )
        [ Attr.class "ball"
        ]
        []


renderProps index =
    [ 
        
        -- Animator.x (toFloat index * 200)
     Animator.rotation (toFloat index )
        --, Animator.px "border-width" (toFloat index * 3)
    ,   Animator.x (toFloat index * 200)
           
    -- , 
    -- , Animator.color "background-color" 
    --     (if modBy 2 index  == 0 then
    --         red
    --     else     
    --         blue
    --     )
    ]
        


{- Less Exciting Stuff

   Below here is some content and a stylesheet.

-}


loremIpsum : Html msg
loremIpsum =
    Html.div []
        [ Html.div []
            [ Html.text "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
            ]
        , Html.div []
            [ Html.text "Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source. Lorem Ipsum comes from sections 1.10.32 and 1.10.33 of \"de Finibus Bonorum et Malorum\" (The Extremes of Good and Evil) by Cicero, written in 45 BC. This book is a treatise on the theory of ethics, very popular during the Renaissance. The first line of Lorem Ipsum, \"Lorem ipsum dolor sit amet..\", comes from a line in section 1.10.32."
            ]
        , Html.div []
            [ Html.text "The standard chunk of Lorem Ipsum used since the 1500s is reproduced below for those interested. Sections 1.10.32 and 1.10.33 from \"de Finibus Bonorum et Malorum\" by Cicero are also reproduced in their exact original form, accompanied by English versions from the 1914 translation by H. Rackham."
            ]
        ]


stylesheet : Html msg
stylesheet =
    Html.node "style"
        []
        [ text """@import url('https://fonts.googleapis.com/css?family=Roboto&display=swap');

a {
    text-decoration: none;
    color: black;
}

a:visited {
    text-decoration: none;
    color: black;
}
.root {
    width: 100%;
    height: 1000px;
    font-size: 16px;
    user-select: none;
    padding: 50px;
    font-family: 'Roboto', sans-serif;
}
.page-row {
    display: flex;
    flex-direction: row;

    padding: 100px;

}
.page {
    width: 500px;
    padding: 48px;
    border: 1px solid black;
    border-radius: 2px;
    flex-shrink: 0;
    background-color: white;
}


.ball {
    box-sizing: border-box;
    position:absolute;
    width: 100px;
    height: 100px;
    border-radius: 20%;
    background-color:red;
    border-color: black;
    border-style: solid;
}

.ball-target {
    position:absolute;
    width: 100px;
    height: 100px;
    border-radius: 50%;
    border: 2px dashed black;
}

"""
        ]
