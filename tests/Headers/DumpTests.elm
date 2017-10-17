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
X-Foo = '''
Bar, \\
Baz, \\
Qux'''
"""
