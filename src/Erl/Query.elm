module Erl.Query
    exposing
        ( parse
        , toString
        , add
        , set
        , remove
        , getValuesForKey
        )

{-| Functions to work with a Query record


# Parse

@docs parse


# Mutation helpers

@docs add, set, remove


# Serialize

@docs toString


# Other helpers

@docs getValuesForKey

-}

import Erl.Types as Types
import Http
import String


{-| Parse a query string

    Erl.Query.parse "?a=1&b=2&a=3" == [("a", "1"), ("b", "2"), ("a", "1")]

-}
parse : String -> Types.Query
parse queryString =
    let
        trimmed =
            queryString
                |> String.split "?"
                |> String.join ""

        splitted =
            String.split "&" trimmed
    in
        if String.isEmpty trimmed then
            []
        else
            List.map queryStringElementToTuple splitted



-- "a=1" --> ("a", "1")


queryStringElementToTuple : String -> ( String, String )
queryStringElementToTuple element =
    let
        splitted =
            String.split "=" element

        first =
            Maybe.withDefault "" (List.head splitted)

        firstDecoded =
            Http.decodeUri first |> Maybe.withDefault ""

        second =
            Maybe.withDefault "" (List.head (List.drop 1 splitted))

        secondDecoded =
            Http.decodeUri second |> Maybe.withDefault ""
    in
        ( firstDecoded, secondDecoded )


{-| Convert to a string, this includes '?'

    Erl.Query.toString query == "?a=1&b=2"

-}
toString : Types.Query -> String
toString query =
    let
        encodedTuples =
            List.map (\( x, y ) -> ( Http.encodeUri x, Http.encodeUri y )) query

        parts =
            List.map (\( a, b ) -> a ++ "=" ++ b) encodedTuples
    in
        if List.isEmpty query then
            ""
        else
            "?" ++ (String.join "&" parts)


{-| Adds key/value in query string

    Erl.Query.add key value query

This doesn't replace existing keys, so if this is a duplicated this key is just added.

-}
add : String -> String -> Types.Query -> Types.Query
add key val =
    List.reverse
        >> (::) ( key, val )
        >> List.reverse


{-| Set key/value in query string, removes any existing one if necessary.

    Erl.Query.set key value query

-}
set : String -> String -> Types.Query -> Types.Query
set key val query =
    let
        without =
            remove key query
    in
        add key val without


{-| Removes key from query string

    Erl.Query.remove key query

-}
remove : String -> Types.Query -> Types.Query
remove key query =
    List.filter (\( k, v ) -> k /= key) query


{-| Gets values for a key in the query

    url = Erl.parse "?a=1&b=2&a=3"

    Erl.Query.getQueryValuesForKey "a" url.query

    == ["1", "3"]

-}
getValuesForKey : String -> Types.Query -> List String
getValuesForKey key =
    List.filter (\( k, _ ) -> k == key)
        >> List.map Tuple.second
