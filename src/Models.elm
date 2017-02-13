module Models exposing (..)

import Html exposing (..)
import Messages exposing (Msg)
import Routing


type alias Rules =
    { text : String
    , updatedText : String
    }


type alias Model =
    { history : List (Maybe Routing.Route)
    , rules : Rules
    }


type alias Binding =
    { model : Rules
    , message : String -> Messages.Msg
    , docsLink : String
    , placeholder : String
    , parse : Rules -> Html Msg
    }
