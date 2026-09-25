module Loading exposing (main)

{-| Animating loading states!

It's pretty common to have a type, usually called RemoteData, to represent a piece of data that's been requested from the server.

It generally looks something like this:

    type RemoteData error data
        = NotAsked
        | Loading
        | Failure error
        | Success data

This example will show you how to:

1.  Animate a "resting" state, so that when we're at a state of `Loading`, a a loading animation is occurring.
2.  Animate content that's already been deleted! In this case we can show the previously retrieved comment, even whil we're in a loading state.
3.  How to debug animations with `Animator.Css.explain`. This will show you a bounding box for an element, as well as it's center of rotation.

Search for (1), (2), and (3) respectively to see the code!

-}

import Animator
import Animator.Timeline as Timeline
import Browser
import Browser.Events
import Color
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Process
import Task
import Time


type RemoteData error data
    = NotAsked
    | Loading
    | Failure error
    | Success data


type alias Model =
    { comment :
        Timeline.Timeline (RemoteData String String)
    }


main =
    Browser.document
        { init =
            \() ->
                ( { comment =
                        Timeline.init NotAsked
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                if Timeline.isRunning model.comment then
                    Browser.Events.onAnimationFrame Tick

                else
                    Sub.none
        }


type Msg
    = Tick Time.Posix
    | AskServerForNewComment
    | NewCommentFromServer String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | comment = Timeline.update newTime model.comment }
            , Cmd.none
            )

        AskServerForNewComment ->
            ( { model
                | comment =
                    model.comment
                        |> Timeline.to (Animator.ms 300) Loading
              }
            , Task.perform (always (NewCommentFromServer "Howdy partner!"))
                (Process.sleep (2 * 1000))
            )

        NewCommentFromServer comment ->
            ( { model
                | comment =
                    model.comment
                        |> Timeline.to (Animator.ms 300) (Success comment)
              }
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    { title = "Animator - Loading"
    , body =
        [ stylesheet
        , div [ Attr.class "root" ]
            [ div
                [ Attr.class "viewport"
                ]
                [ viewComment model.comment
                , Html.div
                    [ Attr.style "display" "flex"
                    , Attr.style "flex-direction" "row"
                    , Attr.style "justify-content" "space-between"
                    , Attr.style "width" "100%"
                    ]
                    [ Html.button
                        [ Attr.class "button"
                        , Events.onClick AskServerForNewComment
                        ]
                        [ Html.text "Load comment" ]
                    ]
                ]
            ]
        ]
    }


viewComment : Timeline.Timeline (RemoteData String String) -> Html Msg
viewComment commentTimeline =
    Animator.div
        (Animator.onTimeline commentTimeline
            (\state ->
                case state of
                    Loading ->
                        [ Animator.color "background-color"
                            (Color.rgb 0.9 0.9 0.9)
                        , Animator.color "color"
                            (Color.rgb 0.5 0.5 0.5)
                        , Animator.color "border-color"
                            (Color.rgb 0.5 0.5 0.5)
                        ]

                    _ ->
                        [ Animator.color "background-color"
                            (Color.rgb 1 1 1)
                        , Animator.color "color"
                            (Color.rgb 0 0 0)
                        , Animator.color "border-color"
                            (Color.rgb 0 0 0)
                        ]
            )
        )
        [ Attr.style "position" "relative"
        , Attr.style "display" "flex"
        , Attr.style "flex-direction" "column"
        , Attr.style "justify-content" "space-between"
        , Attr.style "font-size" "16px"
        , Attr.style "width" "100%"
        , Attr.style "height" "100px"
        , Attr.style "padding" "24px"
        , Attr.style "box-sizing" "border-box"
        , Attr.style "border" "1px solid black"
        , Attr.style "border-radius" "3px"
        ]
        [ loadingIndicator commentTimeline
        , case Timeline.current commentTimeline of
            NotAsked ->
                Html.div [ Attr.style "color" "#CCC" ] [ Html.text "No comment loaded" ]

            Loading ->
                Html.div []
                    -- (2) - We can still show the previous comment!
                    --       If we're loading a new one, we'll grey it out.
                    [ case Timeline.previous commentTimeline of
                        Success text ->
                            Html.div
                                [ Attr.style "display" "flex"
                                , Attr.style "flex-direction" "row"
                                , Attr.style "filter" "grayscale(1)"
                                ]
                                [ viewThinking
                                , Html.text text
                                ]

                        _ ->
                            Html.div [] [ Html.text "Loading..." ]
                    ]

            Failure error ->
                Html.div [] [ Html.text error ]

            Success text ->
                Html.div
                    [ Attr.style "display" "flex"
                    , Attr.style "flex-direction" "row"
                    ]
                    [ -- viewCowboy
                      -- -- (3) - This `debug` field will turn on `Animator.Css.explain` for the elements animated in the `viewCowboy` function.
                      -- -- See the other note labeled (3) to get a better idea of what that means.
                      -- { debug = False
                      -- , timeline = comment
                      -- }
                      -- ,
                      Html.text text
                    ]
        ]


loadingIndicator : Timeline.Timeline (RemoteData error success) -> Html msg
loadingIndicator loading =
    Html.div
        [ Attr.class "loading-indicator"
        ]
        [ viewBlinkingCircle 0.2 loading
        , viewBlinkingCircle 0.3 loading
        , viewBlinkingCircle 0.4 loading
        ]


viewBlinkingCircle offset timeline =
    Animator.div
        (Animator.onTimeline timeline <|
            \state ->
                case state of
                    Loading ->
                        -- (1) - When we're loading, use a sine wave to oscillate between 0.05 and 1
                        --       Becase we want multiple blinking circles, we can als "shift" this wave over by some amount.
                        --       A shift of 0 means the value starts at 0.05.
                        --       A shift of 1 would mean the value starts at the "end", at 1.
                        --       Then we tell it to take 700ms for a full loop
                        [ -- Animator.wave 0.05 1
                          -- |> Animator.shift offset
                          -- |> Animator.loop (Animator.millis 700)
                          Animator.opacity 1
                        ]

                    _ ->
                        [ Animator.opacity 0
                        ]
        )
        [ Attr.class "circle"
        ]
        []


viewThinking : Html msg
viewThinking =
    Html.div
        [ Attr.class "thinking"
        ]
        [ Html.text "🤔"
        ]



{- STYLESHEET -}


pink : String
pink =
    "rgb(240, 0, 245)"


stylesheet : Html msg
stylesheet =
    Html.node "style"
        []
        [ text """@import url('https://fonts.googleapis.com/css?family=Roboto&display=swap');



.circle {
    width: 12px;
    height: 12px;
    border-radius: 6px;
    background-color: rgb(240, 0, 245);
    margin-right: 8px;
}

.thinking {
    display: flex;
    flex-direction: center;
    transform: translateY(-6px);
    width: 72px;
    font-size: 24px;
    margin-right: 12px;
    justify-content: center;
}
.loading-indicator {
    display: flex;
    width: 100%;
    height: 100%;
    left: 0;
    top: 0;
    position: absolute;
    box-sizing: border-box;
    flex-direction: row;
    justify-content: center;
    align-items: center;
}

.root {
    width: 100%;
    font-size: 48px;
    user-select: none;
    padding: 50px;
    box-sizing: border-box;
    font-family: 'Roboto', sans-serif;
    padding: 200px;
}
.viewport {
    display: flex;
    flex-direction: column;
    align-items: flex-start;
    justify-content: center;
    width: 400px;

}
.button {
    background-color: rgb(240, 0, 245);
    padding: 8px 12px;
    border: none;
    border-radius: 2px;
    color: white;
    cursor: pointer;
    margin-top: 20px;

}
.cowboy {
    display: flex;
    transform: translateY(-6px);
    width: 72px;
    font-size: 24px;
    margin-right: 12px;
}
"""
        ]
