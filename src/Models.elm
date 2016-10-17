module Models exposing (..)

import Routing


type alias Rules =
    { text : String
    , updatedText : String
    }


type alias Model =
    { route : Routing.Route
    , rules : Rules
    }


initialModel : Routing.Route -> Model
initialModel route =
    { route = route
    , rules = Rules "" ""
    }
