module Redirects.Dump exposing (dump, stringRule)

import Dict exposing (Dict)
import Erl
import List
import Redirects.Parser exposing (Rule, Conditions)


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
    "\n[[redirects]]"
        ++ "\nfrom = \""
        ++ (Erl.toString rule.origin)
        ++ "\""
        ++ (mapQuery rule.params "query")
        ++ "\nto = \""
        ++ (Erl.toString rule.target.url)
        ++ "\""
        ++ "\nstatus = "
        ++ (String.toLower (toString rule.target.status))
        ++ "\nforce = "
        ++ (String.toLower (toString rule.target.force))
        ++ (mapConditions rule.conditions)
        ++ "\n"


mapQuery : Conditions -> String -> String
mapQuery conditions name =
    if Dict.isEmpty conditions then
        ""
    else
        "\n" ++ name ++ " = " ++ (conditionsToString conditions)


mapConditions : Conditions -> String
mapConditions conditions =
    if Dict.isEmpty conditions then
        ""
    else
        let
            signature =
                Dict.get "Signed" conditions

            filter =
                Dict.remove "Signed" conditions

            ctos =
                mapQuery filter "conditions"

            sign =
                signToString signature
        in
            ctos ++ sign


signToString : Maybe String -> String
signToString signature =
    case signature of
        Nothing ->
            ""

        Just value ->
            "\nsigned = \"" ++ value ++ "\""


conditionsToString : Dict String String -> String
conditionsToString conditions =
    let
        dict =
            Dict.toList conditions
                |> List.map conditionToString
                |> String.join ", "
    in
        "{" ++ dict ++ "}"


conditionToString : ( String, String ) -> String
conditionToString tuple =
    let
        ( key, value ) =
            tuple
    in
        key ++ " = \"" ++ value ++ "\""
