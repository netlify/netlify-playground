module View exposing (..)

import Html exposing (Html, div, text, main', p)
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
            [ class "central-message" ]
            [ div [ class "title" ]
                [ text "This is not the place you're looking for" ]
            ]
        ]


homeView : Rules -> Html Msg
homeView model =
    div []
        [ Partials.pageHeader model Nothing
        , main'
            [ class "central-message" ]
            [ div [ class "title" ]
                [ text "PLAY - Netlify's Playground" ]
            , p [] [ text "Version 1.0.0" ]
            , p [] [ text "By David Calavera et al." ]
            , p [] [ text "Modified by hello@netlify.com" ]
            , p [] [ text "Play is open source and freely distributable" ]
            ]
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
