module Mario exposing (main)

{-| -}

import Animator
import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attr
import Json.Decode as Decode
import Task
import Time



{--}
type alias Model =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , window : Window
    , gamepad : GamePad
    , mario : Animator.Timeline Mario
    }


type alias GamePad =
    { left : Pressed
    , right : Pressed
    , jump : Pressed
    , run : Pressed
    , duck : Pressed
    }


type Pressed
    = NotPressed
    | StartPressed
    | HeldFor Milliseconds


type alias Milliseconds =
    Float


type alias Keys =
    { x : Int
    , y : Int
    }


type Arrow
    = ArrowDown
    | ArrowUp
    | ArrowLeft
    | ArrowRight


type alias Window =
    { width : Int
    , height : Int
    }


type Mario
    = Mario Action Direction


type Action
    = Running
    | Walking
    | Standing
    | Ducking
    | Jumping


type Direction
    = Left
    | Right


type Msg
    = Tick Time.Posix
    | Frame Float
    | Pressed Button
    | Released Button
    | WindowSize Int Int


type Button
    = GoLeft
    | GoRight
    | Duck
    | Jump
    | Run


main =
    Browser.document
        { init =
            \() ->
                ( init
                , Browser.Dom.getViewport
                    |> Task.attempt
                        (\viewportResult ->
                            case viewportResult of
                                Ok viewport ->
                                    WindowSize
                                        (round viewport.scene.width)
                                        (round viewport.scene.height)

                                Err err ->
                                    WindowSize
                                        (round 800)
                                        (round 600)
                        )
                )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Model
init =
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , window = { width = 800, height = 500 }
    , gamepad =
        { left = NotPressed
        , right = NotPressed
        , jump = NotPressed
        , run = NotPressed
        , duck = NotPressed
        }
    , mario = Animator.init (Mario Standing Right)
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mario =
    case msg of
        Tick newTime ->
            ( mario
                |> Animator.update newTime animator
            , Cmd.none
            )

        Frame dt ->
            ( mario
                |> holdButtons dt
                |> gravity (dt / 10)
                |> jump mario.gamepad
                |> walk mario.gamepad
                |> physics (dt / 10)
                |> updateSprites
            , Cmd.none
            )

        Pressed button ->
            ( { mario
                | gamepad =
                    applyButtonToGamepad button True mario.gamepad
              }
            , Cmd.none
            )

        Released button ->
            ( { mario
                | gamepad =
                    applyButtonToGamepad button False mario.gamepad
              }
            , Cmd.none
            )

        WindowSize width height ->
            ( { mario
                | window =
                    { width = width
                    , height = height
                    }
              }
            , Cmd.none
            )


{-| -}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ animator
            |> Animator.toSubscription Tick model
        , Browser.Events.onResize WindowSize
        , Browser.Events.onKeyDown (Decode.map Pressed decodeButton)
        , Browser.Events.onKeyUp (Decode.map Released decodeButton)
        , Browser.Events.onAnimationFrameDelta Frame
        ]


animator : Animator.Animator Model
animator =
    Animator.animator
        -- we tell the animator how to get the checked timeline using .checked
        -- and we tell the animator how to update that timeline with updateChecked
        |> Animator.with .mario (\mario m -> { m | mario = mario })


decodeButton : Decode.Decoder Button
decodeButton =
    Decode.andThen toButton
        (Decode.field "key" Decode.string)


toButton : String -> Decode.Decoder Button
toButton string =
    case string of
        "ArrowLeft" ->
            Decode.succeed GoLeft

        "ArrowRight" ->
            Decode.succeed GoRight

        " " ->
            Decode.succeed Jump

        "ArrowDown" ->
            Decode.succeed Duck

        "Shift" ->
            Decode.succeed Run

        _ ->
            Decode.fail "Skip"


holdButtons : Float -> Model -> Model
holdButtons dt model =
    let
        gamepad =
            model.gamepad
    in
    { model
        | gamepad = holdButtonsOnGamepad dt gamepad
    }


holdButtonsOnGamepad dt gamepad =
    { left = hold dt gamepad.left
    , right = hold dt gamepad.right
    , jump = hold dt gamepad.jump
    , run = hold dt gamepad.run
    , duck = hold dt gamepad.duck
    }


hold dt pressed =
    case pressed of
        NotPressed ->
            NotPressed

        StartPressed ->
            HeldFor dt

        HeldFor existingDt ->
            HeldFor (existingDt + dt)


applyButtonToGamepad : Button -> Bool -> GamePad -> GamePad
applyButtonToGamepad button pressed gamepad =
    case button of
        GoLeft ->
            { gamepad
                | left =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        GoRight ->
            { gamepad
                | right =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        Duck ->
            { gamepad
                | duck =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        Jump ->
            { gamepad
                | jump =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }

        Run ->
            { gamepad
                | run =
                    if pressed then
                        StartPressed

                    else
                        NotPressed
            }


view : Model -> Browser.Document Msg
view model =
    { title = "Mario - Elm Animator"
    , body =
        [ stylesheet

        -- , viewSpriteInfo model.sprites
        -- , viewSpriteSheet model (Tuple.first model.sprites)
        , Html.div
            [ Attr.style "position" "fixed"
            , Attr.style "left" "0"
            , Attr.style "top" "0"
            , Attr.style "width" (String.fromInt model.window.width ++ "px")
            , Attr.style "height" (String.fromInt model.window.height ++ "px")
            ]
            [ Html.div
                [ Attr.class "positioner"
                , Attr.style "position" "absolute"
                , Attr.style "top" (String.fromFloat ((toFloat model.window.height / 2) - model.y) ++ "px")
                , Attr.style "left" (String.fromFloat model.x ++ "px")
                ]
                [ viewSprite
                    (Animator.step model.mario <|
                        \(Mario action direction) ->
                            let
                                frame mySprite =
                                    case direction of
                                        Left ->
                                            Animator.frame mySprite

                                        Right ->
                                            Animator.frame { mySprite | flipX = True }
                            in
                            case action of
                                Walking ->
                                    Animator.framesWith
                                        { transition = frame sprite.tail.stand
                                        , resting =
                                            Animator.cycle
                                                (Animator.fps 15)
                                                [ frame sprite.tail.step1
                                                , frame sprite.tail.step2
                                                , frame sprite.tail.stand
                                                ]
                                        }

                                Running ->
                                    Animator.framesWith
                                        { transition = frame sprite.tail.standArms
                                        , resting =
                                            Animator.cycle
                                                (Animator.fps 30)
                                                [ frame sprite.tail.runStep1
                                                , frame sprite.tail.runStep2
                                                , frame sprite.tail.standArms
                                                ]
                                        }

                                Jumping ->
                                    frame sprite.tail.jump

                                Ducking ->
                                    frame sprite.tail.duck

                                Standing ->
                                    frame sprite.tail.stand
                    )
                ]
            ]
        ]
    }


viewSprite : Box -> Html msg
viewSprite box =
    Html.div []
        [ Html.div
            [ Attr.style "position" "absolute"
            , Attr.style "top" (String.fromInt box.adjustY ++ "px")
            , Attr.style "left" (String.fromInt box.adjustX ++ "px")
            , Attr.style "width" (String.fromInt box.width ++ "px")
            , Attr.style "height" (String.fromInt box.height ++ "px")
            , Attr.style "background-image" "url('./images/mario-sprites.png')"
            , Attr.style "background-repeat" "no-repeat"
            , Attr.style "transform-origin" "30% 50%"
            , Attr.style "transform"
                (if box.flipX then
                    "scaleX(-1) scale(2)"

                 else
                    "scaleX(1) scale(2)"
                )
            , Attr.style "background-position"
                ("-"
                    ++ (String.fromInt box.x ++ "px -")
                    ++ (String.fromInt box.y ++ "px")
                )
            , Attr.class "pixel-art"
            ]
            []
        , Html.span
            [ Attr.style "background-color" "green"
            , Attr.style "width" "1px"
            , Attr.style "height" "100%"
            , Attr.style "left" "6px"
            , Attr.style "position" "absolute"
            ]
            []
        , Html.span
            [ Attr.style "background-color" "green"
            , Attr.style "width" "1px"
            , Attr.style "height" "100%"
            , Attr.style "left" "18px"
            , Attr.style "position" "absolute"
            ]
            []
        ]


isHeld : Pressed -> Bool
isHeld pressed =
    case pressed of
        NotPressed ->
            False

        StartPressed ->
            True

        HeldFor _ ->
            True


walk : GamePad -> Model -> Model
walk pad mario =
    let
        run yes x =
            if yes then
                x * 2.0

            else
                x

        newVx =
            if isHeld pad.left && isHeld pad.right then
                0

            else if isHeld pad.left then
                run (isHeld pad.run) -1.8

            else if isHeld pad.right then
                run (isHeld pad.run) 1.8

            else
                0
    in
    { mario
        | vx = newVx
    }


jump : GamePad -> Model -> Model
jump pad mario =
    if pad.jump == StartPressed && mario.vy == 0 then
        { mario | vy = 6.0 }

    else
        mario


gravity : Float -> Model -> Model
gravity dt mario =
    { mario
        | vy =
            if mario.y > 0 then
                mario.vy - dt / 4

            else
                0
    }


physics : Float -> Model -> Model
physics dt mario =
    { mario
        | x =
            (mario.x + dt * mario.vx)
                |> min (toFloat mario.window.width - 40)
                |> max 0
        , y = max 0 (mario.y + dt * mario.vy)
    }


updateSprites : Model -> Model
updateSprites model =
    let
        current =
            Animator.current model.mario

        direction =
            if model.vx > 0 then
                Right

            else if model.vx < 0 then
                Left

            else
                case current of
                    Mario _ currentDirection ->
                        currentDirection

        action =
            if model.y /= 0 then
                Jumping

            else if model.vx /= 0 then
                if abs model.vx > 2 then
                    Running

                else
                    Walking

            else if isHeld model.gamepad.duck then
                Ducking

            else
                Standing

        newMario =
            Mario action direction
    in
    if current /= newMario then
        { model
            | mario =
                model.mario
                    |> Animator.immediately newMario
        }

    else
        model



{- Sprite Sheet -}


type alias Box =
    { x : Int
    , y : Int
    , width : Int
    , height : Int
    , adjustX : Int
    , adjustY : Int
    , flipX : Bool
    , flipY : Bool
    }


sprite =
    { tail =
        { stand =
            { x = 0
            , y = 240
            , width = 27
            , height = 30
            , adjustX = 4
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , step1 =
            { x = 30
            , y = 240
            , width = 27
            , height = 30
            , adjustX = 3
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , step2 =
            { x = 60
            , y = 240
            , width = 27
            , height = 30
            , adjustX = 4
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , jump =
            { x = 90
            , y = 240
            , width = 27
            , height = 30
            , adjustX = 4
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , duck =
            { x = 120
            , y = 235
            , width = 27
            , height = 30
            , adjustX = 5
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , pivot =
            { x = 150
            , y = 240
            , width = 27
            , height = 30
            , adjustX = 0
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , kick =
            { x = 180
            , y = 240
            , width = 27
            , height = 30
            , adjustX = -1
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , bum =
            { x = 208
            , y = 239
            , width = 20
            , height = 30
            , adjustX = 3
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , standArms =
            { x = 0
            , y = 280
            , width = 25
            , height = 30
            , adjustX = 4
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , runStep1 =
            { x = 25
            , y = 280
            , width = 27
            , height = 30
            , adjustX = 3
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , runStep2 =
            { x = 52
            , y = 280
            , width = 25
            , height = 30
            , adjustX = 4
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , runJump1 =
            { x = 329
            , y = 280
            , width = 27
            , height = 30
            , adjustX = 3
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , runJump2 =
            { x = 359
            , y = 280
            , width = 27
            , height = 30
            , adjustX = 3
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , runJump3 =
            { x = 389
            , y = 280
            , width = 27
            , height = 30
            , adjustX = 3
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , fall1 =
            { x = 268
            , y = 280
            , width = 27
            , height = 30
            , adjustX = 3
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        , fall2 =
            { x = 300
            , y = 280
            , width = 27
            , height = 30
            , adjustX = 3
            , adjustY = 0
            , flipX = False
            , flipY = False
            }
        }
    }


stylesheet : Html msg
stylesheet =
    Html.node "style"
        []
        [ Html.text """@import url('https://fonts.googleapis.com/css?family=Roboto&display=swap');

body, html {
    margin: 0;
    padding:0;
    border:0;
    display:block;
    position: relative;
    width: 100%;
    height: 100%;
}

.pixel-art {
    image-rendering: pixelated;
    image-rendering: -moz-crisp-edges;
    image-rendering: crisp-edges;
}
"""
        ]
