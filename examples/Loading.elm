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

  - Animate a "resting" state, so that when we're at a state of `Loading`, a loading spinner is rotating.
  - Animate content that's already been deleted!

-}

import Animator
import Animator.Css
import Animator.Inline
import Browser
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
        Animator.Timeline (RemoteData String String)
    }


main =
    Browser.document
        { init =
            \() ->
                ( { comment =
                        Animator.init NotAsked
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                animator
                    |> Animator.toSubscription Tick model
        }


{-| -}
animator : Animator.Animator Model
animator =
    Animator.animator
        -- *NOTE*  We're using `the Animator.Css.with` instead of the normal one.
        -- This one only needs to update
        |> Animator.Css.with
            .comment
            (\newComment model ->
                { model | comment = newComment }
            )


type Msg
    = Tick Time.Posix
    | AskServerForNewComment
    | NewCommentFromServer String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( model
                |> Animator.update newTime animator
            , Cmd.none
            )

        AskServerForNewComment ->
            ( { model
                | comment =
                    model.comment
                        |> Animator.to Loading
              }
            , Task.perform (always (NewCommentFromServer "Howdy partner!"))
                (Process.sleep (2 * 1000))
            )

        NewCommentFromServer comment ->
            let
                _ =
                    Debug.log "new comment" comment
            in
            ( { model
                | comment =
                    model.comment
                        |> Animator.to (Success comment)
              }
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    { title = "Animator - Loading"
    , body =
        [ Html.node "style"
            []
            [ text """@import url('https://fonts.googleapis.com/css?family=Roboto&display=swap');"""
            ]
        , div
            [ Attr.style "width" "100%"
            , Attr.style "font-size" "48px"
            , Attr.style "user-select" "none"
            , Attr.style "padding" "50px"
            , Attr.style "box-sizing" "border-box"
            , Attr.style "font-family" "'Roboto', sans-serif"
            ]
            [ div
                [ Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "align-items" "flex-start"
                , Attr.style "justify-content" "center"
                , Attr.style "padding" "200px"
                ]
                [ viewComment model.comment
                , Html.button
                    [ Attr.style "background-color" "rgb(240, 0, 245)"
                    , Attr.style "padding" "8px 12px"
                    , Attr.style "border" "none"
                    , Attr.style "border-radius" "2px"
                    , Attr.style "color" "white"
                    , Attr.style "cursor" "pointer"
                    , Attr.style "margin-top" "20px"
                    , Events.onClick AskServerForNewComment
                    ]
                    [ Html.text "Load comment" ]
                ]
            ]
        ]
    }


viewComment comment =
    Html.div
        [ Attr.style "display" "flex"
        , Attr.style "flex-direction" "row"
        , Attr.style "width" "600px"
        , Attr.style "height" "100px"
        ]
        [ viewStatus True comment
        , viewCommentContent comment
        ]


viewCommentContent comment =
    case Animator.current comment of
        NotAsked ->
            Html.div [] [ Html.text "No comment loaded" ]

        Loading ->
            Html.div []
                -- We can still show the previous comment!
                -- if we're loading a new one, we'll grey it out.
                [ case Animator.previous comment of
                    Success text ->
                        Html.div [] [ Html.text text ]

                    _ ->
                        Html.div [] [ Html.text "Loading..." ]
                ]

        Failure error ->
            Html.div [] [ Html.text error ]

        Success text ->
            Html.div [] [ Html.text text ]


viewStatus debug comment =
    Animator.Css.animated comment
        [ Animator.Css.explain debug
        , Animator.Css.transformWith
            { rotationAxis =
                { x = 0
                , y = 0
                , z = 1
                }
            , origin =
                Animator.Css.offset
                    -25
                    -20
                    0
                    0
            }
          <|
            \state ->
                case state of
                    NotAsked ->
                        Animator.Css.xy
                            { x = 0
                            , y = 0
                            }

                    Loading ->
                        Animator.Css.rotating (Animator.seconds 5)

                    Failure error ->
                        Animator.Css.xy
                            { x = 0
                            , y = 0
                            }

                    Success text ->
                        Animator.Css.xy
                            { x = 0
                            , y = 0
                            }
                            |> Animator.Css.lookAt
                                { x = 0.7
                                , y = 0.2
                                , z = 0.8
                                }
        ]
        [ Attr.style "width" "100px"
        ]
        [ case Animator.current comment of
            NotAsked ->
                Html.text ""

            Loading ->
                Html.text "\u{1F914}"

            Failure error ->
                Html.text "😞"

            Success text ->
                Html.div [ Attr.style "display" "flex" ]
                    [ Html.span [] [ Html.text "👉" ]
                    , Html.span [] [ Html.text "\u{1F920}" ]
                    , Html.span
                        [ Attr.style "transform" "scaleX(-1) rotate(270deg)"
                        ]
                        [ Html.text "☝️" ]
                    ]
        ]
