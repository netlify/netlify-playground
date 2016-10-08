module Redirects.Tests exposing (..)

import Redirects.Parser exposing (..)
import Redirects.Parser exposing (Rule, Target)
import Test exposing (..)
import Expect
import String
import Erl
import Dict


all : Test
all =
    describe "The Redirects Parser"
        [ describe "parsing status codes"
            [ test "giberish" <|
                \() -> Expect.equal ( 301, False ) (parseStatus "asdfasdf")
            , test "301" <|
                \() -> Expect.equal ( 301, False ) (parseStatus "301")
            , test "301!" <|
                \() -> Expect.equal ( 301, True ) (parseStatus "301!")
            , test "302" <|
                \() -> Expect.equal ( 302, False ) (parseStatus "302")
            , test "303" <|
                \() -> Expect.equal ( 303, False ) (parseStatus "303")
            , test "200" <|
                \() -> Expect.equal ( 200, False ) (parseStatus "200")
            , test "404" <|
                \() -> Expect.equal ( 404, False ) (parseStatus "404")
            ]
        , describe "filtering rules"
            [ test "giberish" <|
                \() -> Expect.equal (Err "invalid origin URL") (filterRule "asdasdfasdf")
            , test "only origin URL" <|
                \() -> Expect.equal (Err "target URL is missing") (filterRule "/foo")
            , test "only origin URL and params" <|
                \() -> Expect.equal (Err "target URL is missing") (filterRule "/foo bar=baz qux=quux")
            , test "origin URL and target URL" <|
                \() ->
                    let
                        rule =
                            filterRule "/foo /bar"

                        tuple =
                            ( (Erl.parse "/foo"), (Erl.parse "/bar"), 301 )
                    in
                        expectParsedRule rule <|
                            \rule -> Expect.equal tuple ( rule.origin, rule.target.url, rule.target.status )
            , test "origin URL and target URL with protocols" <|
                \() ->
                    let
                        rule =
                            filterRule "http://foo.com/foo http://foo.com/bar"

                        tuple =
                            ( (Erl.parse "http://foo.com/foo"), (Erl.parse "http://foo.com/bar"), 301 )
                    in
                        expectParsedRule rule <|
                            \rule -> Expect.equal tuple ( rule.origin, rule.target.url, rule.target.status )
            , test "origin URL, parameters and target URL" <|
                \() ->
                    let
                        rule =
                            filterRule "/foo qux=quux /bar"
                    in
                        expectParsedRule rule <|
                            \rule -> Expect.equal (Dict.fromList [ ( "qux", "quux" ) ]) rule.params
            , test "origin URL, parameters, target URL and status" <|
                \() ->
                    let
                        rule =
                            filterRule "/foo qux=quux /bar 302!"
                    in
                        expectParsedRule rule <|
                            \rule -> Expect.equal ( 302, True ) ( rule.target.status, rule.target.force )
            , test "origin URL, parameters, target URL, status and filters" <|
                \() ->
                    let
                        rule =
                            filterRule "/foo qux=quux /bar 302! Country=en,es Language=es"
                    in
                        expectParsedRule rule <|
                            \rule -> Expect.equal (Dict.fromList [ ( "Country", "en,es" ), ( "Language", "es" ) ]) (rule.filters)
            ]
        ]


expectParsedRule : Result String Rule -> (Rule -> Expect.Expectation) -> Expect.Expectation
expectParsedRule maybeRule expectation =
    case maybeRule of
        Err msg ->
            Expect.fail ("should not return a parsing error, got: " ++ msg)

        Ok rule ->
            expectation rule
