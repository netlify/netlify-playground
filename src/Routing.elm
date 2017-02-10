module Routing exposing (..)

import String
import Navigation
import UrlParser exposing (s, top)


type Route
    = Home
    | Redirects
    | Headers


route : UrlParser.Parser (Route -> a) a
route =
    UrlParser.oneOf
        [ UrlParser.map Home top
        , UrlParser.map Redirects (s "redirects")
        , UrlParser.map Headers (s "headers")
        ]
