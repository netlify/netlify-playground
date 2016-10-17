module Main exposing (..)

import Navigation
import Messages exposing (Msg(..))
import Models exposing (Model, Rules, initialModel)
import View exposing (view)
import Routing exposing (Route)


init : Result String Route -> ( Model, Cmd Msg )
init result =
    let
        currentRoute =
            Routing.routeFromResult result
    in
        ( initialModel currentRoute, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RulesChanged newRules ->
            { model | rules = Rules newRules model.rules.updatedText } ! []

        ParseRedirects newRules ->
            { model | rules = Rules model.rules.text newRules } ! []

        ShowRedirects ->
            ( model, Navigation.newUrl "#redirects" )


urlUpdate : Result String Route -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    let
        currentRoute =
            Routing.routeFromResult result
    in
        ( { model | route = currentRoute }, Cmd.none )


main : Program Never
main =
    Navigation.program Routing.parser
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }
