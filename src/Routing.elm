module Routing exposing (..)

import String
import Navigation
import UrlParser exposing (s, top)


type Route
    = Home
    | Redirects


route : UrlParser.Parser (Route -> a) a
route =
    UrlParser.oneOf
        [ UrlParser.map Home top
        , UrlParser.map Redirects (s "redirects")
        ]
