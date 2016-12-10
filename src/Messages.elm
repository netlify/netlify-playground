module Messages exposing (..)

import Navigation
import Redirects.Messages


type Msg
    = NewUrl String
    | UrlChange Navigation.Location
    | ParseRedirects String
    | RulesChanged String
