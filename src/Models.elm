module Models exposing (..)

import Routing


type alias Rules =
    { text : String
    , updatedText : String
    }


type alias Model =
    { history : List (Maybe Routing.Route)
    , rules : Rules
    }
