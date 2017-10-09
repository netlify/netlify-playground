module Tests exposing (suite)

import Test exposing (Test, describe)
import Headers.DumpTests
import Headers.ParserTests
import Redirects.DumpTests
import Redirects.ParserTests


suite : Test
suite =
    describe "playground"
        [ Headers.ParserTests.suite
        , Headers.DumpTests.suite
        , Redirects.ParserTests.suite
        , Redirects.DumpTests.suite
        ]
