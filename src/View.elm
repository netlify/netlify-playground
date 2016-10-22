module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))
import Models exposing (Model)
import Routing exposing (Route(..))
import Models exposing (Rules)
import Partials
import Redirects.View


view : Model -> Html Msg
view model =
    case model.route of
        RedirectsRoute ->
            Redirects.View.render model.rules

        HomeRoute ->
            homeView model.rules

        NotFoundRoute ->
            notFoundView model.rules


notFoundView : Rules -> Html Msg
notFoundView model =
    div []
        [ Partials.pageHeader model Nothing Nothing
        , main'
            [ class "central-message" ]
            [ div [ class "title" ]
                [ text "This is not the place you're looking for" ]
            ]
        ]


homeView : Rules -> Html Msg
homeView model =
    div []
        [ Partials.pageHeader model Nothing Nothing
        , main'
            [ class "central-message" ]
            [ div [ class "title" ]
                [ text "PLAY - Netlify's Playground" ]
            , p [] [ text "Version 1.0.0" ]
            , p [] [ text "By David Calavera et al." ]
            , p [] [ text "Modified by hello@netlify.com" ]
            , p []
                [ span []
                    [ text "Netlify's Playground is "
                    , a [ href "https://github.com/netlify/netlify-playground" ]
                        [ text "open source" ]
                    , text " and freely distributable"
                    ]
                ]
            , p [ class "help" ]
                [ span []
                    [ text "go to "
                    , a
                        [ onClick ShowRedirects ]
                        [ text "redirects" ]
                    , text " to test your _redirects rules"
                    ]
                ]
            ]
        ]
