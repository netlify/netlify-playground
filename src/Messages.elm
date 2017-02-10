module Messages exposing (..)

import Navigation
import Redirects.Messages


type Msg
    = NewUrl String
    | UrlChange Navigation.Location
    | RulesChanged String
    | ParseRedirects String
    | ParseHeaders String
