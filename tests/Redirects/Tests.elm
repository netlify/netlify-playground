module Redirects.Tests exposing (..)

import Redirects.Parser exposing (..)
import Redirects.Parser exposing (ParseResult, Rule, Target)
import Test exposing (..)
import Expect
import String
import Erl
import Dict


suite : Test
suite =
    describe "The Redirects Parser"
        [ describe "parsing status codes"
            [ test "giberish" <|
                \() -> Expect.equal (Err "the status code is invalid, it should be a number or a number with an exclamation mark after") (parseStatus "asdfasdf")
            , test "301" <|
                \() -> Expect.equal (Ok ( 301, False )) (parseStatus "301")
            , test "301!" <|
                \() -> Expect.equal (Ok ( 301, True )) (parseStatus "301!")
            , test "302" <|
                \() -> Expect.equal (Ok ( 302, False )) (parseStatus "302")
            , test "303" <|
                \() -> Expect.equal (Ok ( 303, False )) (parseStatus "303")
            , test "200" <|
                \() -> Expect.equal (Ok ( 200, False )) (parseStatus "200")
            , test "404" <|
                \() -> Expect.equal (Ok ( 404, False )) (parseStatus "404")
            ]
        , describe "filtering rules"
            [ test "giberish" <|
                \() ->
                    let
                        rule =
                            filterRule "asdasdfasdf"
                    in
                        Expect.equal (Err "invalid origin URL, it should either start with / or be an absolute URL") rule.result
            , test "only origin URL" <|
                \() ->
                    let
                        rule =
                            filterRule "/foo"
                    in
                        Expect.equal (Err "the target URL is missing, it should either start with / or be an absolute URL") rule.result
            , test "only origin URL and params" <|
                \() ->
                    let
                        rule =
                            filterRule "/foo bar=baz qux=quux"
                    in
                        Expect.equal (Err "the target URL is missing, it should either start with / or be an absolute URL") rule.result
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
                            \rule -> Expect.equal ( 302, True, False ) ( rule.target.status, rule.target.force, rule.target.proxy )
            , test "origin URL, parameters, target URL, status and filters" <|
                \() ->
                    let
                        rule =
                            filterRule "/foo qux=quux /bar 302! Country=en,es Language=es"
                    in
                        expectParsedRule rule <|
                            \rule -> Expect.equal (Dict.fromList [ ( "Country", "en,es" ), ( "Language", "es" ) ]) (rule.filters)
            , test "origin URL and proxy target" <|
                \() ->
                    let
                        rule =
                            filterRule "/foo http://foo.com/bar 200"
                    in
                        expectParsedRule rule <|
                            \rule -> Expect.equal ( 200, True ) ( rule.target.status, rule.target.proxy )
            , test "ignore inline comments" <|
                \() ->
                    let
                        rule =
                            filterRule "/foo /bar 302! # Country=en,es Language=es"
                    in
                        expectParsedRule rule <|
                            \rule -> Expect.equal Dict.empty rule.filters
            , test "/blog post=:id  /news post=:id 302" <|
                \() ->
                    let
                        rule =
                            filterRule "/blog post=:id /news post=:id 302"
                    in
                        Expect.equal (Err "the status code is invalid, it should be a number or a number with an exclamation mark after") rule.result
            ]
        ]


expectParsedRule : ParseResult -> (Rule -> Expect.Expectation) -> Expect.Expectation
expectParsedRule rule expectation =
    case rule.result of
        Err msg ->
            Expect.fail ("should not return a parsing error, got: " ++ msg)

        Ok rule ->
            expectation rule
