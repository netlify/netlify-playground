module Headers.Tests exposing (..)

import Array
import Dict exposing (Dict)
import Expect
import Headers.Parser exposing (..)
import List
import Test exposing (..)


suite : Test
suite =
    describe "The Headers Parser"
        [ test "simple rules" simpleRulesTest
        , test "multikey rules" multikeyRulesTest
        , test "ignore headers without paths" headersWithoutPathTest
        , test "ignore comments" rulesWithCommentsTest
        ]


simpleRulesTest =
    let
        rules =
            """
/
  X-Foo: Bar
/*
  X-Bar: Baz
"""

        response =
            filterRules rules

        expected =
            [ (Rule "/" (Dict.fromList [ ( "X-Foo", [ "Bar" ] ) ]) [])
            , (Rule "/*" (Dict.fromList [ ( "X-Bar", [ "Baz" ] ) ]) [])
            ]
    in
        \() -> Expect.equal expected response.rules


multikeyRulesTest =
    let
        rules =
            """
/
  X-Foo: Bar
  X-Bar: Baz
  X-Foo: Qux
/*
  X-Bar: Baz
"""

        response =
            filterRules rules

        expected =
            [ (Rule "/" (Dict.fromList [ ( "X-Foo", [ "Bar", "Qux" ] ), ( "X-Bar", [ "Baz" ] ) ]) [])
            , (Rule "/*" (Dict.fromList [ ( "X-Bar", [ "Baz" ] ) ]) [])
            ]
    in
        \() -> Expect.equal expected response.rules


headersWithoutPathTest =
    let
        rules =
            """
  X-Foo: Bar
  X-Bar: Baz
  X-Foo: Qux
  X-Bar: Baz
"""

        response =
            filterRules rules
    in
        \() -> Expect.equal True (List.isEmpty response.rules)


rulesWithCommentsTest =
    let
        rules =
            """
/
# Foo header
  X-Foo: Bar
/*
# Bar header
  X-Bar: Baz
"""

        response =
            filterRules rules

        expected =
            [ (Rule "/" (Dict.fromList [ ( "X-Foo", [ "Bar" ] ) ]) [])
            , (Rule "/*" (Dict.fromList [ ( "X-Bar", [ "Baz" ] ) ]) [])
            ]
    in
        \() -> Expect.equal expected response.rules
