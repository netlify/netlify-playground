module Headers.DumpTests exposing (suite)

import Dict
import Erl
import Expect
import Headers.Dump exposing (dump)
import Headers.Parser exposing (Rule, Values)
import Test exposing (..)


suite : Test
suite =
    describe "Test Headers Dump"
        [ test "simple rule" <|
            \() -> Expect.equal simpleRules dumpSimpleRules
        , test "rule with multiple headers" <|
            \() -> Expect.equal rulesWithMultipleHeaders dumpRuleWithMultipleHeaders
        , test "rule with quotes" <|
            \() -> Expect.equal rulesWithQuotes dumpRuleWithQuotes
        , test "rule with multilines" <|
            \() -> Expect.equal rulesWithMultilines dumpRuleWithMultilines
        ]


dumpSimpleRules =
    let
        rules =
            [ (Rule "/" (Dict.fromList [ ( "X-Foo", [ "Bar" ] ) ]) [])
            , (Rule "/*" (Dict.fromList [ ( "X-Bar", [ "Baz" ] ), ( "X-Baz", [ "Qux" ] ) ]) [])
            ]
    in
        dump rules


dumpRuleWithMultipleHeaders =
    let
        rules =
            [ (Rule "/" (Dict.fromList [ ( "X-Foo", [ "Bar", "Baz", "Qux" ] ) ]) [])
            ]
    in
        dump rules


dumpRuleWithQuotes =
    let
        rules =
            [ (Rule "/*.txt" (Dict.fromList [ ( "Link", [ "\"<http://mydomain.com/plaintext.css>;rel=stylesheet;type=text/css;media=all\"" ] ) ]) [])
            ]
    in
        dump rules


dumpRuleWithMultilines =
    let
        rules =
            [ (Rule "/*.txt"
                (Dict.fromList
                    [ ( "Link"
                      , [ "\"<http://mydomain.com/plaintext.css>;rel=stylesheet;type=text/css;media=all\""
                        , "\"<http://mydomain.com/plaintext.css>;rel=stylesheet;type=text/css;media=all\""
                        ]
                      )
                    ]
                )
                []
              )
            ]
    in
        dump rules


simpleRules =
    """
[[headers]]
for = "/"
[headers.values]
X-Foo = "Bar"

[[headers]]
for = "/*"
[headers.values]
X-Bar = "Baz"
X-Baz = "Qux"
"""


rulesWithMultipleHeaders =
    """
[[headers]]
for = "/"
[headers.values]
X-Foo = [
"Bar",
"Baz",
"Qux"
]
"""


rulesWithQuotes =
    """
[[headers]]
for = "/*.txt"
[headers.values]
Link = "<http://mydomain.com/plaintext.css>;rel=stylesheet;type=text/css;media=all"
"""


rulesWithMultilines =
    """
[[headers]]
for = "/*.txt"
[headers.values]
Link = [
"<http://mydomain.com/plaintext.css>;rel=stylesheet;type=text/css;media=all",
"<http://mydomain.com/plaintext.css>;rel=stylesheet;type=text/css;media=all"
]
"""
