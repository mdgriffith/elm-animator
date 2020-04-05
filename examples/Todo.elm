module Todo exposing (main)

{-| Let's make a todo list!

And do the following

    1.  Completing items are animated to the end of the list
    2.  Fade elements out when they're deleted

Why is this tricky?

    1.  We need bounding boxes in order to do our reordering animation.  But we can only get bounding boxes after something has been rendered.
    2.  How can we animate something if it's been deleted?!
        Oh, wait, we can go back in time. Duh.




-}

import Animator
import Animator.Css
import Animator.Inline
import Browser
import Browser.Dom
import Color
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Process
import Task
import Time


type alias Entry =
    { description : String
    , done : Bool
    , id : Int

    -- We capture bounding box information from the browser
    -- https://package.elm-lang.org/packages/elm/browser/latest/Browser-Dom#Element
    , box : Maybe Browser.Dom.Element
    }


type alias Model =
    { entries :
        Animator.Timeline (List Entry)
    , mostRecentId : Int
    , field : String
    }


main =
    Browser.document
        { init =
            \() ->
                ( { entries =
                        Animator.init
                            []
                  , field = ""
                  , mostRecentId = 0
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
        |> Animator.Css.watching
            .entries
            (\newEntries model ->
                { model | entries = newEntries }
            )


type Msg
    = Tick Time.Posix
    | Delete Int
    | Complete Int
    | FieldUpdated String
    | AddEntry
    | NewBoundingBox Int Browser.Dom.Element


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( model
                |> Animator.update newTime animator
            , Cmd.none
            )

        Delete id ->
            ( model
            , Cmd.none
            )

        Complete id ->
            ( model
            , Cmd.none
            )

        AddEntry ->
            ( model, Cmd.none )

        NewBoundingBox id box ->
            ( model, Cmd.none )


getBoundingBoxes items =
    Cmd.batch (List.map getBBox (Animator.current items))


getBBox item =
    case 
    Browser.Dom.get


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
            , Attr.style "padding" "200px"
            ]
            [ div
                [ Attr.style "display" "flex"
                , Attr.style "flex-direction" "column"
                , Attr.style "align-items" "flex-start"
                , Attr.style "justify-content" "center"
                , Attr.style "width" "400px"
                ]
                [ Html.div []
                    [ Html.div [] []
                    , Html.div
                        [ Attr.style "display" "flex"
                        , Attr.style "flex-direction" "row"
                        , Attr.style "justify-content" "space-between"
                        , Attr.style "width" "100%"
                        ]
                        [ Html.button
                            [ Attr.style "background-color" pink
                            , Attr.style "padding" "8px 12px"
                            , Attr.style "border" "none"
                            , Attr.style "border-radius" "2px"
                            , Attr.style "color" "white"
                            , Attr.style "cursor" "pointer"
                            , Attr.style "margin-top" "20px"
                            , Events.onClick AddEntry
                            ]
                            [ Html.text "Load comment" ]
                        ]
                    ]
                , viewEntries model.entries
                ]
            ]
        ]
    }


pink =
    "rgb(240, 0, 245)"


viewEntries entries =
    Html.text ""
