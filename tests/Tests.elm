module Tests exposing (suite)

import Test exposing (Test, describe)
import Headers.Tests
import Redirects.Tests


suite : Test
suite =
    describe "playground"
        [ Headers.Tests.suite
        , Redirects.Tests.suite
        ]
