module Basic exposing (main)

{-| 

-}

import Animator
import Browser
import Color
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Time


type alias Model = {}


main =
    Browser.sandbox
        { init =
            {}
        , view = view
        , update = update     
        }


type Msg
    =  Check Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        Check newChecked ->
            model
           
           


view : Model -> Html Msg
view model =
    div
        [ Attr.class "root"
        ]
        [ stylesheet
        , box (Animator.spinning (Animator.ms 2000))
        , box (Animator.spinning (Animator.ms 1500))
        , box (Animator.spinning (Animator.ms 1000))
        ]


box animation =
    Animator.div animation
        [ Attr.class "box"
        ]
        [

        ]

    



stylesheet : Html msg
stylesheet =
    Html.node "style"
        []
        [ text """@import url('https://fonts.googleapis.com/css?family=Roboto&display=swap');

.root {
    display: flex;

}
.root > * {
    margin-right: 20px;
}

.box {
    box-sizing: border-box;
    position: relative;
    width: 100px;
    height: 100px;
    border-radius: 20%;
    background-color:red;
    border-color: black;
    border-style: solid;
}

"""
        ]
