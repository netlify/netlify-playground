module Headers.Dump exposing (dump, stringRule)

import Dict exposing (Dict)
import Headers.Parser exposing (Rule, Values)


dump : List Rule -> String
dump rules =
    concatRules rules ""


concatRules : List Rule -> String -> String
concatRules rules response =
    case rules of
        [] ->
            response

        head :: tail ->
            concatRule head tail response


concatRule : Rule -> List Rule -> String -> String
concatRule head tail response =
    let
        newResponse =
            response ++ (stringRule head)
    in
        concatRules tail newResponse


stringRule : Rule -> String
stringRule rule =
    "\n[[headers]]"
        ++ "\nfor = \""
        ++ rule.path
        ++ "\""
        ++ "\n[headers.values]"
        ++ (stringValues rule.valid)
        ++ "\n"


stringValues : Dict String Values -> String
stringValues values =
    Dict.map (\key value -> stringValue value) values
        |> Dict.toList
        |> stringList


stringValue : Values -> String
stringValue values =
    case values of
        [] ->
            ""

        [ header ] ->
            quote header

        head :: tail ->
            let
                rest =
                    (List.map quote tail) |> String.join ",\n"
            in
                "[\n"
                    ++ (quote head)
                    ++ ",\n"
                    ++ rest
                    ++ "\n]"


stringList : List ( String, String ) -> String
stringList values =
    List.map (\( key, value ) -> "\n" ++ key ++ " = " ++ value) values
        |> String.join ""


quote : String -> String
quote string =
    if String.startsWith "\"" string && String.endsWith "\"" string then
        string
    else
        "\"" ++ string ++ "\""
