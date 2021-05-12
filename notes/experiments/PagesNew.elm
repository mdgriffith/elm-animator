module PagesNew exposing (main)

{-| Animated page transitions!

This example is meant to show a few things.

    1.  That page transitions are just like animating any other state, we'll just create an `Animator.Timeline Page` and animate with that.
    2.  How to use CSS keyframes by using the `Animator.Css` module
    3.  How to handle routing so that the url changes as your transition.

-}

import Animator
import Animator.Css2
import Browser
import Browser.Events
import Browser.Navigation
import Color
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Time
import Url
import Url.Builder
import Url.Parser exposing ((</>))


{-| -}
type alias Model =
    { page : Animator.Timeline Page
    , navKey : Browser.Navigation.Key
    , needsUpdate : Bool
    }


main =
    Browser.application
        { init =
            \() url navKey ->
                let
                    initialPage =
                        Url.Parser.parse urlParser url
                            |> Maybe.withDefault NotFound
                in
                ( { page = Animator.init initialPage
                  , navKey = navKey
                  , needsUpdate = False
                  }
                , Cmd.none
                )
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Sub.batch
                    [ animator
                        |> Animator.toSubscription Tick model
                    ]
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChanged
        }



{- URL Handling -}


type Page
    = Home
    | About
    | Blog
    | NotFound


urlParser : Url.Parser.Parser (Page -> a) a
urlParser =
    Url.Parser.oneOf
        [ Url.Parser.map Home Url.Parser.top
        , Url.Parser.map Blog (Url.Parser.s "blog")
        , Url.Parser.map About (Url.Parser.s "about")
        ]


pageToUrl : Page -> String
pageToUrl page =
    case page of
        Home ->
            Url.Builder.absolute [] []

        About ->
            Url.Builder.absolute [ "about" ] []

        Blog ->
            Url.Builder.absolute [ "blog" ] []

        NotFound ->
            Url.Builder.absolute [ "not-found" ] []


animator : Animator.Animator Model
animator =
    Animator.animator
        -- *NOTE*  We're using `the Animator.Css.watching` instead of `Animator.watching`.
        -- Instead of asking for a constant stream of animation frames, it'll only ask for one
        -- and we'll render the entire css animation in that frame.
        |> Animator.Css2.watching .page
            (\newPage model ->
                { model | page = newPage }
            )



{- UPDATING -}


type Msg
    = Tick Time.Posix
    | ClickedLink Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( Animator.update newTime animator model
            , Cmd.none
            )

        ClickedLink request ->
            case request of
                Browser.Internal url ->
                    -- Note -  Ideally, starting a new animation with `toNewPage` would only happen in `UrlChanged`
                    -- which occurs immediately after this message if we use `Browser.Navigation.pushUrl`
                    --
                    -- However there seems to be a bug in elm where a subscription to animationFrame fails to fire
                    -- if we start a new animation just in `UrlChanged`.
                    -- Note, this seems to be a sepcial case with routing
                    ( toNewPage url model
                    , Browser.Navigation.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        UrlChanged url ->
            -- This should be te only place we need to use `toNewPage`.  See above note.
            ( toNewPage url model
            , Cmd.none
            )


toNewPage : Url.Url -> Model -> Model
toNewPage url model =
    let
        newPage =
            Url.Parser.parse urlParser url
                |> Maybe.withDefault NotFound
    in
    { model
        | page =
            model.page
                -- full page animations involve moving some large stuff.
                -- in that case using a slower duration than normal is a good place to start.
                |> Animator.go Animator.verySlowly newPage
    }



{- Actually viewing our pages! -}


view : Model -> Browser.Document Msg
view model =
    { title = "Animator - Page Transitions"
    , body =
        [ stylesheet
        , div
            [ Attr.class "root"
            ]
            [ nav []
                [ link Home "Home"
                , link Blog "Blog"
                , link About "About"
                ]
            , div
                [ Attr.class "page-row"
                ]
                [ viewPage model.page
                    Home
                    { title = "The home page"
                    , content = loremIpsum
                    }

                -- , viewPage model.page
                --     Blog
                --     { title = "Blog"
                --     , content = loremIpsum
                --     }
                -- , viewPage model.page
                --     About
                --     { title = "About"
                --     , content = loremIpsum
                --     }
                ]
            ]
        ]
    }


viewPage : Animator.Timeline Page -> Page -> { title : String, content : Html msg } -> Html Msg
viewPage timeline currentPage { title, content } =
    let
        wrapInLink html =
            if Animator.current timeline == currentPage then
                html

            else
                Html.a
                    [ Attr.href (pageToUrl currentPage)
                    , Attr.style "cursor" "pointer"
                    ]
                    [ html ]
    in
    Animator.Css2.div timeline
        (\page ->
            if currentPage == page then
                [ -- Animator.Css2.opacity 1
                  -- ,
                  Animator.Css2.scale 1.5
                , Animator.Css2.x 500

                -- , Animator.Css2.px "margin-left" 1801
                -- , Animator.Css2.px "margin-right" 1802
                , Animator.Css2.px
                    "border-width"
                    5

                -- , Animator.Css2.color "background-color" (Color.rgb255 255 255 0)
                ]

            else
                [ --  Animator.Css2.opacity 0.4
                  Animator.Css2.px "border-width"
                    1

                -- ,
                -- ,
                , Animator.Css2.scale 1
                , Animator.Css2.x 0

                -- , Animator.Css2.color "background-color" (Color.rgb255 255 255 255)
                ]
        )
        [ Attr.class "page" ]
        [ Html.h2 [] [ Html.text title ]
        , loremIpsum
        ]
        |> wrapInLink



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


link : Page -> String -> Html msg
link page label =
    Html.a
        [ Attr.href (pageToUrl page)
        , Attr.style "margin-right" "12px"
        ]
        [ Html.text label ]


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
    align-items: center;
    justify-content: center;
    padding: 100px;
    transform: scale(0.2);
}
.page {
    width: 500px;
    padding: 48px;
    border: 1px solid black;
    border-radius: 2px;
    flex-shrink: 0;
    background-color: white;
}

"""
        ]
