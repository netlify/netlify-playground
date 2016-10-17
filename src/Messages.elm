module Messages exposing (..)

import Redirects.Messages


type Msg
    = ShowRedirects
    | ParseRedirects String
    | RulesChanged String
