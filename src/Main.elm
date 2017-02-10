module Main exposing (..)

import Navigation
import Messages exposing (Msg(..))
import Models exposing (Model, Rules)
import View exposing (view)
import Routing exposing (Route, route)
import UrlParser


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        history =
            UrlParser.parsePath route location
    in
        ( { history = [ history ], rules = Rules "" "" }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUrl url ->
            ( model, Navigation.newUrl url )

        UrlChange location ->
            ( { model
                | history = UrlParser.parsePath route location :: model.history
                , rules = Rules "" ""
              }
            , Cmd.none
            )

        RulesChanged newRules ->
            { model | rules = Rules newRules model.rules.updatedText } ! []

        ParseRedirects newRules ->
            { model | rules = Rules model.rules.text newRules } ! []

        ParseHeaders newRules ->
            { model | rules = Rules model.rules.text newRules } ! []


main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
