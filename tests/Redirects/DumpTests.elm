module Redirects.DumpTests exposing (suite)

import Dict
import Erl
import Expect
import Redirects.Dump exposing (dump)
import Redirects.Parser exposing (Conditions, Rule, Target)
import Test exposing (..)


suite : Test
suite =
    describe "Test Redirects Dump"
        [ test "simple rule" <|
            \() -> Expect.equal simpleRule dumpSimpleRule
        , test "rule with parameters" <|
            \() -> Expect.equal ruleWithParameters dumpRuleWithParameters
        , test "rule with conditions" <|
            \() -> Expect.equal ruleWithConditions dumpRuleWithConditions
        , test "multiple rules" <|
            \() -> Expect.equal multipleRules dumpMultipleRules
        , test "rule with signature" <|
            \() -> Expect.equal ruleWithSignature dumpRuleWithSignature
        ]


dumpSimpleRule =
    let
        target =
            (Target (Erl.parse "/foo") 301 False False)

        rule =
            (Rule (Erl.parse "/") Dict.empty target Dict.empty)
    in
        dump [ rule ]


dumpRuleWithParameters =
    let
        target =
            (Target (Erl.parse "/foo") 301 False False)

        params =
            Dict.fromList [ ( "foo", "bar" ), ( "baz", "qux" ) ]

        rule =
            (Rule (Erl.parse "/") params target Dict.empty)
    in
        dump [ rule ]


dumpRuleWithConditions =
    let
        target =
            (Target (Erl.parse "/foo") 301 False False)

        params =
            Dict.fromList [ ( "foo", "bar" ), ( "baz", "qux" ) ]

        rule =
            (Rule (Erl.parse "/") params target params)
    in
        dump [ rule ]


dumpMultipleRules =
    let
        target =
            (Target (Erl.parse "/foo") 301 False False)

        rule1 =
            (Rule (Erl.parse "/") Dict.empty target Dict.empty)

        rule2 =
            (Rule (Erl.parse "/") Dict.empty target Dict.empty)
    in
        dump [ rule1, rule2 ]


dumpRuleWithSignature =
    let
        target =
            (Target (Erl.parse "/foo") 301 False False)

        conditions =
            Dict.fromList [ ( "Signed", "API_SIGNATURE_TOKEN" ) ]

        rule =
            (Rule (Erl.parse "/") Dict.empty target conditions)
    in
        dump [ rule ]


simpleRule =
    """
[[redirects]]
from = "/"
to = "/foo"
status = 301
force = false
"""


ruleWithParameters =
    """
[[redirects]]
from = "/"
query = {baz = "qux", foo = "bar"}
to = "/foo"
status = 301
force = false
"""


ruleWithConditions =
    """
[[redirects]]
from = "/"
query = {baz = "qux", foo = "bar"}
to = "/foo"
status = 301
force = false
conditions = {baz = "qux", foo = "bar"}
"""


multipleRules =
    """
[[redirects]]
from = "/"
to = "/foo"
status = 301
force = false

[[redirects]]
from = "/"
to = "/foo"
status = 301
force = false
"""


ruleWithSignature =
    """
[[redirects]]
from = "/"
to = "/foo"
status = 301
force = false
signed = "API_SIGNATURE_TOKEN"
"""
