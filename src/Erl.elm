module Erl
    exposing
        ( addQuery
        , appendPathSegments
        , clearQuery
        , extractHash
        , extractHost
        , extractPath
        , extractPort
        , extractProtocol
        , extractQuery
        , getQueryValuesForKey
        , new
        , parse
        , Query
        , queryToString
        , removeQuery
        , setQuery
        , toString
        , toAbsoluteString
        , Url
        )

{-| Library for parsing and constructing URLs


# Types

@docs Url, Query


# Parse

@docs parse


# Parse helpers

@docs extractHash, extractHost, extractPath, extractProtocol, extractPort, extractQuery


# Construct

@docs new


# Mutation helpers

@docs addQuery, setQuery, removeQuery, clearQuery, appendPathSegments


# Serialize

@docs toString, toAbsoluteString


# Serialization helpers

@docs queryToString


# Other helpers

@docs getQueryValuesForKey

-}

import Dict
import Erl.Query
import Erl.Types as Types
import Http
import Regex
import String exposing (..)


-- TYPES


{-| List of tuples that holds the keys and values in the query string
-}
type alias Query =
    Types.Query


{-| Record that holds url attributes
-}
type alias Url =
    { protocol : String
    , username : String
    , password : String
    , host : List String
    , port_ : Int
    , path : List String
    , hasLeadingSlash : Bool
    , hasTrailingSlash : Bool
    , hash : String
    , query : Query
    }



-- UTILS


notEmpty : String -> Bool
notEmpty str =
    not (isEmpty str)



-- "aa#bb" --> "bb"


rightFrom : String -> String -> String
rightFrom delimiter str =
    let
        parts =
            split delimiter str
    in
        case List.length parts of
            0 ->
                ""

            1 ->
                ""

            _ ->
                parts
                    |> List.reverse
                    |> List.head
                    |> Maybe.withDefault ""


rightFromLeftMost : String -> String -> String
rightFromLeftMost delimiter str =
    let
        parts =
            split delimiter str
    in
        case List.length parts of
            0 ->
                ""

            1 ->
                ""

            _ ->
                parts
                    |> List.tail
                    |> Maybe.withDefault []
                    |> join delimiter


rightFromOrSame : String -> String -> String
rightFromOrSame delimiter str =
    let
        parts =
            split delimiter str
    in
        parts
            |> List.reverse
            |> List.head
            |> Maybe.withDefault ""



-- "a/b" -> "a"
-- "a"   => "a"
-- "/b"  => ""


leftFromOrSame : String -> String -> String
leftFromOrSame delimiter str =
    let
        parts =
            split delimiter str
    in
        parts
            |> List.head
            |> Maybe.withDefault ""



-- "a/b" -> "a"
-- "/b"  -> ""
-- "a"   -> ""


leftFrom : String -> String -> String
leftFrom delimiter str =
    let
        parts =
            split delimiter str

        head =
            List.head parts
    in
        case List.length parts of
            0 ->
                ""

            1 ->
                ""

            _ ->
                head |> Maybe.withDefault ""



-- PROTOCOL


{-| Extract the protocol from the url
-}
extractProtocol : String -> String
extractProtocol str =
    let
        parts =
            split "://" str
    in
        case List.length parts of
            1 ->
                ""

            _ ->
                Maybe.withDefault "" (List.head parts)



-- HOST
-- valid host: a-z 0-9 and -


{-| Extract the host from the url
-}
extractHost : String -> String
extractHost str =
    -- if str starts with // or contains :// then we can assume that what follows is the host
    -- if it doesn't then look for tld
    let
        delim =
            schemeHostDelim str
    in
        case delim of
            Just delim ->
                str
                    |> rightFromLeftMost delim
                    |> leftFromOrSame "/"
                    |> leftFromOrSame ":"

            Nothing ->
                let
                    -- Look for something with a dot e.g. host.tld
                    rx =
                        "((\\w|-)+\\.)+(\\w|-)+"
                in
                    str
                        |> leftFromOrSame "/"
                        |> Regex.find (Regex.AtMost 1) (Regex.regex rx)
                        |> List.map .match
                        |> List.head
                        |> Maybe.withDefault ""


schemeHostDelim : String -> Maybe String
schemeHostDelim str =
    if String.startsWith "//" str then
        Just "//"
    else if String.contains "://" str then
        Just "://"
    else
        Nothing


parseHost : String -> List String
parseHost str =
    str
        |> split "."


host : String -> List String
host str =
    parseHost (extractHost str)



-- PORT


{-| Extract the port from the url

If no port is included in the url then Erl will attempt to add a default port:

Http -> 80
Https -> 443
FTP -> 21
SFTP -> 22

-}
extractPort : String -> Int
extractPort str =
    let
        rx =
            Regex.regex ":\\d+"

        res =
            Regex.find (Regex.AtMost 1) rx str
    in
        res
            |> List.map .match
            |> List.head
            |> Maybe.withDefault ""
            |> String.dropLeft 1
            |> toInt
            |> \result ->
                case result of
                    Ok port_ ->
                        port_

                    _ ->
                        case extractProtocol str of
                            "http" ->
                                80

                            "https" ->
                                443

                            "ftp" ->
                                21

                            "sftp" ->
                                22

                            _ ->
                                0



-- PATH


{-| Extract the path from the url
-}
extractPath : String -> String
extractPath str =
    let
        host =
            extractHost str

        delim =
            schemeHostDelim str

        trimmed =
            case delim of
                Just delim ->
                    rightFromLeftMost delim str

                Nothing ->
                    str
    in
        trimmed
            |> leftFromOrSame "?"
            |> leftFromOrSame "#"
            |> Regex.replace (Regex.AtMost 1) (Regex.regex ("^.*?" ++ (Regex.escape host) ++ "(:\\d+)?")) (\_ -> "")


parsePath : String -> List String
parsePath str =
    str
        |> split "/"
        |> List.filter notEmpty
        |> List.map Http.decodeUri
        |> List.map (Maybe.withDefault "")


pathFromAll : String -> List String
pathFromAll str =
    parsePath (extractPath str)


hasLeadingSlashFromAll : String -> Bool
hasLeadingSlashFromAll str =
    Regex.contains (Regex.regex "^/") (extractPath str)


hasTrailingSlashFromAll : String -> Bool
hasTrailingSlashFromAll str =
    Regex.contains (Regex.regex "/$") (extractPath str)



-- FRAGMENT


{-| Extract the hash (hash) from the url
-}
extractHash : String -> String
extractHash str =
    str
        |> split "#"
        |> List.drop 1
        |> List.head
        |> Maybe.withDefault ""


hashFromAll : String -> String
hashFromAll str =
    extractHash str



-- QUERY


{-| Extract the query string from the url
-}
extractQuery : String -> String
extractQuery str =
    let
        query =
            str
                |> split "?"
                |> List.drop 1
                |> List.head
                |> Maybe.withDefault ""
                |> split "#"
                |> List.head
                |> Maybe.withDefault ""
    in
        if String.isEmpty query then
            ""
        else
            "?" ++ query



-- "?a=1&b=2&a=3" --> [("a", "1"), ("b", "2"), ("a", "1")]


parseQuery : String -> Query
parseQuery =
    Erl.Query.parse


queryFromAll : String -> Query
queryFromAll all =
    all
        |> extractQuery
        |> parseQuery


{-| Parse a url string, returns an Erl.Url record

    Erl.parse "http://api.example.com/users/1#x/1?a=1" == Erl.Url{...}

-}
parse : String -> Url
parse str =
    { host = (host str)
    , hash = (hashFromAll str)
    , password = ""
    , path = (pathFromAll str)
    , hasLeadingSlash = (hasLeadingSlashFromAll str)
    , hasTrailingSlash = (hasTrailingSlashFromAll str)
    , port_ = (extractPort str)
    , protocol = (extractProtocol str)
    , query = (queryFromAll str)
    , username = ""
    }



-- TO STRING


{-| Convert to a string only the query component of an url, this includes '?'

    Erl.queryToString url == "?a=1&b=2"

-}
queryToString : Url -> String
queryToString =
    .query
        >> Erl.Query.toString


protocolComponent : Url -> String
protocolComponent url =
    case url.protocol of
        "" ->
            ""

        _ ->
            url.protocol ++ "://"


hostComponent : Url -> String
hostComponent url =
    Http.encodeUri (join "." url.host)


portComponent : Url -> String
portComponent url =
    case url.port_ of
        0 ->
            ""

        80 ->
            ""

        443 ->
            if url.protocol == "https" then
                ""
            else
                ":443"

        _ ->
            ":" ++ (Basics.toString url.port_)


pathComponent : Url -> String
pathComponent url =
    let
        encoded =
            List.map Http.encodeUri url.path

        leadingSlash =
            if hostComponent url /= "" || url.hasLeadingSlash then
                "/"
            else
                ""
    in
        if (List.length url.path) == 0 then
            ""
        else
            leadingSlash ++ (join "/" encoded)


trailingSlashComponent : Url -> String
trailingSlashComponent url =
    if url.hasTrailingSlash == True then
        "/"
    else
        ""


{-| Convert to a string the hash component of an url, this includes '#'

    hashToString url == "#a/b"

-}
hashToString : Url -> String
hashToString url =
    if String.isEmpty url.hash then
        ""
    else
        "#" ++ url.hash


{-| Generate an empty Erl.Url record

    Erl.new ==

    { protocol = ""
    , username = ""
    , password = ""
    , host = []
    , path = []
    , hasLeadingSlash = False
    , hasTrailingSlash = False
    , port_ = 0
    , hash = ""
    , query = Dict.empty
    }

-}
new : Url
new =
    { protocol = ""
    , username = ""
    , password = ""
    , host = []
    , path = []
    , hasLeadingSlash = False
    , hasTrailingSlash = False
    , port_ = 0
    , hash = ""
    , query = []
    }


{-| Clears the current query string

    Erl.clearQuery url

-}
clearQuery : Url -> Url
clearQuery url =
    { url | query = [] }


{-| Adds key/value in query string

    Erl.addQuery key value url

This doesn't replace existing keys, so if this is a duplicated this key is just added.

-}
addQuery : String -> String -> Url -> Url
addQuery key val url =
    { url | query = Erl.Query.add key val url.query }


{-| Set key/value in query string, removes any existing one if necessary.

    Erl.setQuery key value url

-}
setQuery : String -> String -> Url -> Url
setQuery key val url =
    { url | query = Erl.Query.set key val url.query }


{-| Removes key from query string

    Erl.removeQuery key url

-}
removeQuery : String -> Url -> Url
removeQuery key url =
    { url | query = Erl.Query.remove key url.query }


{-| Gets values for a key in the query

    url = Erl.parse "?a=1&b=2&a=3"

    Erl.getQueryValuesForKey "a" url

    == ["1", "3"]

-}
getQueryValuesForKey : String -> Url -> List String
getQueryValuesForKey key url =
    Erl.Query.getValuesForKey key url.query


{-| Append some path segments to a url

    Erl.appendPathSegments ["hello", "world"] url

-}
appendPathSegments : List String -> Url -> Url
appendPathSegments segments url =
    let
        newPath =
            List.append url.path segments
    in
        { url | path = newPath }


{-| Generate a url string from an Erl.Url record

    url = { protocol = "http",
          , username = "",
          , password = "",
          , host = ["www", "foo", "com"],
          , path = ["users", "1"],
          , hasLeadingSlash = False
          , hasTrailingSlash = False
          , port_ = 2000,
          , hash = "a/b",
          , query = Dict.empty |> Dict.insert "q" "1" |> Dict.insert "k" "2"
          }

    Erl.toString url == "http://www.foo.com:2000/users/1?k=2&q=1#a/b"

-}
toString : Url -> String
toString url =
    let
        protocol_ =
            protocolComponent url

        host_ =
            hostComponent url

        port_ =
            portComponent url
    in
        protocol_ ++ host_ ++ port_ ++ toAbsoluteString url


{-| Generate a url that starts at the path

    url = { protocol = "http",
          , username = "",
          , password = "",
          , host = ["www", "foo", "com"],
          , path = ["users", "1"],
          , hasLeadingSlash = False
          , hasTrailingSlash = False
          , port_ = 2000,
          , hash = "a/b",
          , query = Dict.empty |> Dict.insert "q" "1" |> Dict.insert "k" "2"
          }

    Erl.toAbsoluteString url == "/users/1?k=2&q=1#a/b"

-}
toAbsoluteString : Url -> String
toAbsoluteString url =
    let
        path_ =
            pathComponent url

        trailingSlash_ =
            trailingSlashComponent url

        query_ =
            queryToString url

        hash =
            hashToString url
    in
        path_ ++ trailingSlash_ ++ query_ ++ hash
