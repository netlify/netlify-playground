module Redirects.Parser exposing (Rule, Target, Conditions, filterRules, filterRule, parseRule, parseStatus)

import Dict exposing (Dict)
import Erl exposing (Url)
import List
import List.Extra exposing (stripPrefix, takeWhile)
import Regex exposing (regex)
import String exposing (split, startsWith, words, trim, isEmpty)


type alias Rule =
    { origin : Url
    , params : Conditions
    , target : Target
    , filters : Conditions
    }


type alias Target =
    { url : Url
    , status : Int
    , force : Bool
    }


type alias Conditions =
    Dict String String


filterRules : String -> List (Result String Rule)
filterRules rules =
    let
        cleanRules =
            String.lines rules |> List.filterMap (\x -> cleanRule x)
    in
        List.map filterRule cleanRules


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


filterRule : String -> Result String Rule
filterRule rule =
    case rule |> words |> takeWhile notComment of
        [] ->
            Err "invalid origin URL"

        head :: tail ->
            parseRule head tail


parseRule : String -> List String -> Result String Rule
parseRule origin tail =
    if relative origin then
        Err "invalid origin URL"
    else
        let
            originUrl =
                Erl.parse origin

            p =
                takeWhile relative tail

            rest =
                stripPrefix p tail |> Maybe.withDefault []

            params =
                parseConditions p

            targetAndStatus =
                List.take 2 rest

            conditions =
                List.drop 2 rest |> parseConditions

            tr =
                parseTarget targetAndStatus
        in
            case tr of
                Err msg ->
                    Err msg

                Ok target ->
                    Ok (Rule originUrl params target conditions)


parseTarget : List String -> Result String Target
parseTarget ts =
    case ts of
        [] ->
            Err "target URL is missing"

        [ t ] ->
            Ok (newTarget t "301")

        [ t, status ] ->
            Ok (newTarget t status)

        t :: _ ->
            Ok (newTarget t "301")


parseStatus : String -> ( Int, Bool )
parseStatus status =
    if notValidStatus status then
        ( 301, False )
    else
        let
            force =
                String.endsWith "!" status

            code =
                Result.withDefault 301 (String.left 3 status |> String.toInt)
        in
            ( code, force )


newTarget : String -> String -> Target
newTarget target status =
    let
        ( code, force ) =
            parseStatus status
    in
        (Target (Erl.parse target) code force)


parseConditions : List String -> Conditions
parseConditions conditions =
    List.map parseTuple conditions |> Dict.fromList


parseTuple : String -> ( String, String )
parseTuple tuple =
    let
        splitted =
            split "=" tuple

        key =
            Maybe.withDefault tuple (List.head splitted)

        value =
            Maybe.withDefault "" (List.head (List.drop 1 splitted))
    in
        ( key, value )


relative : String -> Bool
relative part =
    not (validUrlPattern part)


validUrlPattern : String -> Bool
validUrlPattern part =
    (Regex.contains (regex "^(/|http)") part)


notComment : String -> Bool
notComment part =
    not (startsWith "#" part)


notValidStatus : String -> Bool
notValidStatus status =
    not (Regex.contains (regex "(200|301|302|303|404)!?") status)
