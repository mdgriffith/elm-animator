module Support.Css exposing (Frame, Keyframes, keyframes, property)

{-| Inspect the small CSS subset emitted by Animator without depending on
whitespace, declaration order, or generated animation names. This deliberately
fails on unexpected syntax rather than letting an empty parse pass a test.
-}

import Dict exposing (Dict)


type alias Frame =
    { percent : Float
    , declarations : Dict String String
    }


type alias Keyframes =
    { name : String
    , frames : List Frame
    }


property : String -> { a | props : List ( String, String ) } -> Maybe String
property name css =
    css.props |> Dict.fromList |> Dict.get name


keyframes : String -> Result String (List Keyframes)
keyframes source =
    case String.split "@keyframes " source of
        prefix :: blocks ->
            if String.trim prefix == "" then
                traverse parseBlock blocks

            else
                Err ("Unexpected CSS before keyframes: " ++ prefix)

        [] ->
            Ok []


parseBlock : String -> Result String Keyframes
parseBlock source =
    case String.split "{" source of
        name :: body ->
            if List.isEmpty body || String.trim name == "" then
                Err ("Invalid keyframes block: " ++ source)

            else
                body
                    |> String.join "{"
                    |> String.split "}"
                    |> List.filter (String.trim >> (/=) "")
                    |> traverse parseFrame
                    |> Result.andThen
                        (\frames ->
                            if List.isEmpty frames then
                                Err ("Empty keyframes block: " ++ name)

                            else
                                Ok { name = String.trim name, frames = frames }
                        )

        [] ->
            Err "Missing keyframes block"


parseFrame : String -> Result String Frame
parseFrame source =
    case String.split "{" source of
        [ selector, body ] ->
            let
                percentage =
                    String.trim selector
            in
            if String.endsWith "%" percentage then
                case String.toFloat (String.dropRight 1 percentage) of
                    Just percent ->
                        body
                            |> String.split ";"
                            |> List.filter (String.trim >> (/=) "")
                            |> traverse parseDeclaration
                            |> Result.map (\props -> { percent = percent, declarations = Dict.fromList props })

                    Nothing ->
                        Err ("Invalid percentage: " ++ percentage)

            else
                Err ("Invalid keyframe selector: " ++ percentage)

        _ ->
            Err ("Invalid frame: " ++ source)


parseDeclaration : String -> Result String ( String, String )
parseDeclaration source =
    case String.split ":" source of
        [ name, value ] ->
            Ok ( String.trim name, String.trim value )

        _ ->
            Err ("Invalid declaration: " ++ source)


traverse : (a -> Result String b) -> List a -> Result String (List b)
traverse parse =
    List.foldr (\item result -> Result.map2 (::) (parse item) result) (Ok [])
