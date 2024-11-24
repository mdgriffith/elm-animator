module Timeline exposing (main)

import Animator
import Animator.Timeline
import Animator.Transition
import Animator.Value as Value
import Browser
import Browser.Events
import Html
import Html.Attributes as Attr
import Html.Events
import Html.Keyed
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { timeline : Animator.Timeline.Timeline Box
    , historyOne : List Float
    , historyTwo : List Float
    }


type Msg
    = NewPosix Time.Posix
    | Start
    | BoxClicked Box


type Box
    = A
    | B
    | C


init : ( Model, Cmd Msg )
init =
    ( { timeline =
            Animator.Timeline.init B

      -- |> Animator.Timeline.to (Animator.ms 1) B
      -- |> Animator.Timeline.update (Time.millisToPosix 1)
      , historyOne = []
      , historyTwo = []
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewPosix posix ->
            ( { model
                | timeline = Animator.Timeline.update posix model.timeline

                -- , historyOne = Animator.Value.float model.timeline Animator.Value.to :: model.historyOne
                -- , historyTwo = Animator.Value.float model.timeline (Animator.Value.withTransition Animator.Transition.linear << Animator.Value.to) :: model.historyTwo
              }
            , Cmd.none
            )

        Start ->
            init

        BoxClicked newBox ->
            ( { model
                | timeline = Animator.Timeline.to (Animator.ms 1000) newBox model.timeline
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    -- if Animator.Timeline.hasChanges model.timeline then
    if Animator.Timeline.isRunning model.timeline then
        Browser.Events.onAnimationFrame NewPosix

    else
        Sub.none


pointer =
    Attr.style "cursor" "pointer"


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ column 40
            [ Attr.style "width" "100%", Attr.style "justify-content" "center", Attr.style "align-items" "center", Attr.style "margin-top" "200px" ]
            [ column 10
                []
                [ -- cursor model.timeline
                  -- ,
                  row []
                    [ box [ pointer, Html.Events.onClick (BoxClicked A) ] []
                    , box [ pointer, Html.Events.onClick (BoxClicked B) ] []
                    , box [ pointer, Html.Events.onClick (BoxClicked C) ] []
                    ]
                ]

            -- , Animator.div
            --     (Animator.onTimeline model.timeline
            --         (\state ->
            --             case state of
            --                 A ->
            --                     [ Animator.opacity 0.5
            --                     , Animator.x -200
            --                     , Animator.y 0
            --                     , Animator.rotation 0.75
            --                     ]
            --                 B ->
            --                     [ Animator.x 0
            --                     , Animator.y 200
            --                     ]
            --                 C ->
            --                     [ Animator.opacity 1
            --                     , Animator.x 200
            --                     , Animator.y 0
            --                     , Animator.rotation 0.5
            --                     ]
            --         )
            --     )
            , let
                xy =
                    Value.xy model.timeline <|
                        \state ->
                            case state of
                                A ->
                                    { x = Value.to -200
                                    , y = Value.to 0
                                    }

                                B ->
                                    { x = Value.to 0
                                    , y = Value.to 200
                                    }

                                C ->
                                    { x = Value.to 200
                                    , y = Value.to 0
                                    }

                rotation =
                    Value.float model.timeline <|
                        \state ->
                            case state of
                                A ->
                                    Value.to 0.75

                                B ->
                                    Value.to 0

                                C ->
                                    Value.to 0.5
              in
              Html.div
                [ Attr.style "width" "100px"
                , Attr.style "height" "100px"
                , Attr.style "border-radius" "12px"
                , Attr.style "background-color" "black"
                , Attr.style "rotate" (String.fromFloat rotation ++ "turn")
                , Attr.style "translate"
                    (String.fromFloat
                        xy.x
                        ++ "px "
                        ++ String.fromFloat
                            xy.y
                        ++ "px"
                    )
                ]
                []
            ]
        ]


cursor : Animator.Timeline.Timeline Box -> Html.Html Msg
cursor timeline =
    -- Animator.div
    Html.div
        -- (Animator.onTimeline timeline
        --     (\state ->
        --         case state of
        --             A ->
        --                 [ Animator.x 40
        --                 ]
        --             B ->
        --                 [ Animator.x 185
        --                 ]
        --             C ->
        --                 [ Animator.x 322
        --                 ]
        --     )
        -- )
        [ Attr.style "width" "10px"
        , Attr.style "height" "10px"
        , Attr.style "border-radius" "50%"
        , Attr.style "background-color" "red"
        , Attr.style "transform"
            ("translateX("
                ++ String.fromFloat
                    (Value.float timeline <|
                        \state ->
                            case state of
                                A ->
                                    Value.to 40

                                B ->
                                    Value.to 185

                                C ->
                                    Value.to 322
                    )
                ++ "px)"
            )
        ]
        []


row : List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
row attributes columns =
    Html.div
        (Attr.style "display" "flex"
            :: Attr.style "flex-direction" "row"
            :: Attr.style "gap" "40px"
            -- :: Attr.style "width" "1000px"
            :: attributes
        )
        columns


column : Int -> List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
column gap attributes columns =
    Html.div
        (Attr.style "display" "flex"
            :: Attr.style "flex-direction" "column"
            :: Attr.style "gap" (String.fromInt gap ++ "px")
            :: attributes
        )
        columns


box : List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg
box attributes content =
    Html.div
        (Attr.style "width" "100px"
            :: Attr.style "height" "100px"
            :: Attr.style "border-radius" "12px"
            :: Attr.style "background-color" "black"
            :: attributes
        )
        content


positionX : Float -> Html.Attribute msg
positionX x =
    Attr.style "transform" ("translateX(" ++ String.fromFloat x ++ "px)")


one =
    { hash = "translate1715042309032d1d1b16777216-t20000-0-0rotate1715042309032d1d1b16842240-r500opacity1715042309032d1d1b16777216-o100"
    , keyframes = ""
    , props = [ ( "transition", "opacity 1000ms cubic-bezier(0.33,-0.35,0.2,1) 0ms, rotate 1000ms cubic-bezier(0.33,-1.62,0.2,1) 0ms, translate 1000ms cubic-bezier(0.4,0,0.2,1) 0ms" ), ( "translate", "200px 0px 0px" ), ( "rotate", "0 0 1 0.5turn" ), ( "opacity", "1" ) ]
    , transition = "opacity 1000ms cubic-bezier(0.33,-0.35,0.2,1) 0ms, rotate 1000ms cubic-bezier(0.33,-1.62,0.2,1) 0ms, translate 1000ms cubic-bezier(0.4,0,0.2,1) 0ms"
    }


two =
    { hash = "translate1715042309565d1d1b16777216-t0-20000-0translate1715042309565d0d0b16843008-t20000-0-0rotate1715042309565d1d1b16777216-r0rotate1715042309565d0d0b16843008-r500opacity1715042309565d1d1b16777472-o100opacity1715042309565d0d0b16843008-o100"
    , keyframes = "@keyframes opacity1715042309565d0d0b16843008-o100 {\n0% {animation-timing-function:cubic-bezier(0.22,0.59,0.53,1);}100% {opacity: 1;}\n}\n@keyframes opacity1715042309565d1d1b16777472-o100 {\n0% {animation-timing-function:cubic-bezier(0.33,1.03,0.2,1);}100% {opacity: 1;}\n}\n@keyframes rotate1715042309565d0d0b16843008-r500 {\n0% {animation-timing-function:cubic-bezier(0.22,0.59,0.53,1);}100% {rotate: 0 0 1 0.5turn;}\n}\n@keyframes rotate1715042309565d1d1b16777216-r0 {\n0% {animation-timing-function:cubic-bezier(0.33,0.09,0.2,1);}100% {rotate: 0 0 1 0turn;}\n}\n@keyframes translate1715042309565d0d0b16843008-t20000-0-0 {\n0% {animation-timing-function:cubic-bezier(0.22,0.59,0.53,1);}100% {translate: 200px 0px 0px;}\n}\n@keyframes translate1715042309565d1d1b16777216-t0-20000-0 {\n0% {animation-timing-function:cubic-bezier(0.4,0,0.2,1);}100% {translate: 0px 200px 0px;}\n}"
    , props = [ ( "animation", "467ms linear 0ms 1 normal forwards running opacity1715042309565d0d0b16843008-o100, 533ms linear 0ms 1 normal forwards running opacity1715042309565d1d1b16777472-o100, 467ms linear 0ms 1 normal forwards running rotate1715042309565d0d0b16843008-r500, 533ms linear 0ms 1 normal forwards running rotate1715042309565d1d1b16777216-r0, 467ms linear 0ms 1 normal forwards running translate1715042309565d0d0b16843008-t20000-0-0, 533ms linear 0ms 1 normal forwards running translate1715042309565d1d1b16777216-t0-20000-0" ) ]
    , transition = ""
    }



-- duration   delay
-- 467ms linear 0ms 1 normal forwards running opacity1715042309565d0d0b16843008-o100
-- 533ms linear 0ms 1 normal forwards running opacity1715042309565d1d1b16777472-o100
-- 467ms linear 0ms 1 normal forwards running rotate1715042309565d0d0b16843008-r500
-- 533ms linear 0ms 1 normal forwards running rotate1715042309565d1d1b16777216-r0
-- 467ms linear 0ms 1 normal forwards running translate1715042309565d0d0b16843008-t20000-0-0
-- 533ms linear 0ms 1 normal forwards running translate1715042309565d1d1b16777216-t0-20000-0
-- @keyframes opacity1715042309565d0d0b16843008-o100 {
--     0% {animation-timing-function:cubic-bezier(0.22,0.59,0.53,1);}100% {opacity: 1;}
-- }
-- @keyframes opacity1715042309565d1d1b16777472-o100 {
--     0% {animation-timing-function:cubic-bezier(0.33,1.03,0.2,1);}100% {opacity: 1;}
-- }
-- @keyframes rotate1715042309565d0d0b16843008-r500 {
--     0% {animation-timing-function:cubic-bezier(0.22,0.59,0.53,1);}100% {rotate: 0 0 1 0.5turn;}
-- }
-- @keyframes rotate1715042309565d1d1b16777216-r0 {
--     0% {animation-timing-function:cubic-bezier(0.33,0.09,0.2,1);}100% {rotate: 0 0 1 0turn;}
-- }
-- @keyframes translate1715042309565d0d0b16843008-t20000-0-0 {
--     0% {animation-timing-function:cubic-bezier(0.22,0.59,0.53,1);}100% {translate: 200px 0px 0px;}
-- }
-- @keyframes translate1715042309565d1d1b16777216-t0-20000-0 {
--     0% {animation-timing-function:cubic-bezier(0.4,0,0.2,1);}100% {translate: 0px 200px 0px;}
-- }
