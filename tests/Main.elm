port module Main exposing (..)

import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)
import Redirects.Tests


main =
    run emit Redirects.Tests.all


port emit : ( String, Value ) -> Cmd msg
