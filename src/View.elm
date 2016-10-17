module View exposing (..)

import Html exposing (Html, div, text, main')
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))
import Models exposing (Model)
import Routing exposing (Route(..))
import Redirects.Parser exposing (parseRedirects)
import Models exposing (Rules)
import Partials


view : Model -> Html Msg
view model =
    case model.route of
        RedirectsRoute ->
            redirectsView model.rules

        HomeRoute ->
            homeView model.rules

        NotFoundRoute ->
            notFoundView model.rules


notFoundView : Rules -> Html Msg
notFoundView model =
    div []
        [ Partials.pageHeader model Nothing
        , main'
            []
            [ text "Not found" ]
        ]


homeView : Rules -> Html Msg
homeView model =
    div []
        [ Partials.pageHeader model Nothing
        , main'
            []
            [ text "Welcome to Netlify's Playground" ]
        ]


redirectsView : Rules -> Html Msg
redirectsView model =
    div [ class "main" ]
        [ Partials.pageHeader model (Just (parseRedirectsButton model))
        , main'
            []
            [ Partials.editor model
            , parseRedirects model
            ]
        ]


parseRedirectsButton : Rules -> Html Msg
parseRedirectsButton rules =
    Html.a
        [ class "nav-item smaller btn-primary nav-cta"
        , onClick (ParseRedirects rules.text)
        ]
        [ text "Parse" ]
