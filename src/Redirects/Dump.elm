module Redirects.Dump exposing (dump, stringRule)

import Dict exposing (Dict)
import Erl
import Http exposing (decodeUri)
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
    let
        fromS =
            Erl.toString rule.origin

        from =
            fromS |> decodeUri |> Maybe.withDefault fromS

        toS =
            Erl.toString rule.target.url

        to =
            toS |> decodeUri |> Maybe.withDefault toS

        status =
            rule.target.status |> toString |> String.toLower

        force =
            rule.target.force |> toString |> String.toLower
    in
        "\n[[redirects]]"
            ++ "\nfrom = \""
            ++ from
            ++ "\""
            ++ (mapQuery rule.params "query")
            ++ "\nto = \""
            ++ to
            ++ "\""
            ++ "\nstatus = "
            ++ status
            ++ "\nforce = "
            ++ force
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
