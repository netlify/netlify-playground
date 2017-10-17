module Headers.Parser exposing (Response, Rule, Values, filterRules)

import Array
import Dict exposing (Dict)
import List
import List.Extra
import Maybe
import Regex
import String exposing (split, startsWith, words, trim, isEmpty)
import Url exposing (validPattern)


type alias Values =
    List String


type alias Response =
    { rules : List Rule
    }


type alias Rule =
    { path : String
    , valid : Dict String Values
    , invalid : List String
    }


filterRules : String -> Response
filterRules rules =
    let
        cleanRules =
            String.lines rules |> List.filterMap cleanRule

        response =
            Response []
    in
        parseRules response cleanRules


parseRules : Response -> List String -> Response
parseRules response lines =
    case lines of
        [] ->
            response

        head :: tail ->
            if validPattern head then
                startRule response head tail
            else
                addHeader response head tail


startRule : Response -> String -> List String -> Response
startRule response head tail =
    let
        newResponse =
            Response (response.rules ++ [ Rule head Dict.empty [] ])
    in
        parseRules newResponse tail


addHeader : Response -> String -> List String -> Response
addHeader response head tail =
    let
        current =
            List.Extra.last response.rules
    in
        case current of
            Nothing ->
                response

            Just rule ->
                appendToLastRule response rule head tail


appendToLastRule : Response -> Rule -> String -> List String -> Response
appendToLastRule response rule head tail =
    let
        newRule =
            appendHeader rule head

        slice =
            Array.slice 0 -1 (Array.fromList response.rules)

        newResponse =
            Response ((Array.toList slice) ++ [ newRule ])
    in
        parseRules newResponse tail


appendHeader : Rule -> String -> Rule
appendHeader rule line =
    let
        values =
            Regex.split (Regex.AtMost 1) (Regex.regex ":") line
    in
        case values of
            [] ->
                { rule | invalid = (rule.invalid ++ [ (trim line) ]) }

            [ _ ] ->
                { rule | invalid = (rule.invalid ++ [ (trim line) ]) }

            head :: tail ->
                addHeaderValue rule head tail


addHeaderValue : Rule -> String -> List String -> Rule
addHeaderValue rule header values =
    let
        key =
            trim header

        current =
            Dict.get key rule.valid
                |> Maybe.withDefault []

        newValue =
            Dict.insert key (current ++ (List.map trim values)) rule.valid
    in
        { rule | valid = newValue }


cleanRule : String -> Maybe String
cleanRule rule =
    let
        trimmed =
            trim rule
    in
        if isEmpty trimmed || startsWith "#" trimmed then
            Nothing
        else
            Just trimmed
