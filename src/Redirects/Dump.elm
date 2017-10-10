module Redirects.Dump exposing (dump, stringRule)

import Dict
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
        ++ "\norigin = \""
        ++ (Erl.toString rule.origin)
        ++ "\""
        ++ "\nparameters = "
        ++ (stringConditions rule.params)
        ++ "\ndestination = \""
        ++ (Erl.toString rule.target.url)
        ++ "\""
        ++ "\nstatus = "
        ++ (String.toLower (toString rule.target.status))
        ++ "\nforce = "
        ++ (String.toLower (toString rule.target.force))
        ++ "\nconditions = "
        ++ (stringConditions rule.conditions)
        ++ "\n"


stringConditions : Conditions -> String
stringConditions conditions =
    let
        dict =
            Dict.toList conditions
                |> List.map stringCondition
                |> String.join ", "
    in
        "{" ++ dict ++ "}"


stringCondition : ( String, String ) -> String
stringCondition tuple =
    let
        ( key, value ) =
            tuple
    in
        key ++ " = \"" ++ value ++ "\""
