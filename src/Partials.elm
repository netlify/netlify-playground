module Partials exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Models exposing (Rules)
import Messages exposing (..)
import Ace


pageHeader : Rules -> Maybe (List ( String, String )) -> Maybe (Html Msg) -> Html Msg
pageHeader model links button =
    header
        [ id "header"
        , class "full-width header"
        ]
        [ nav [ class "header-nav global-nav desktop" ]
            [ a
                [ href "/"
                , class "nav-item logo nav-logo"
                , attribute "data-letters" "Netlify's Playground"
                ]
                [ text "Netlify's Playground" ]
            , a
                [ onClick ShowRedirects
                , class "nav-item"
                , attribute "data-letters" "Redirects"
                ]
                [ text "Redirects" ]
            , showExtras links button
            ]
        ]


showExtras : Maybe (List ( String, String )) -> Maybe (Html Msg) -> Html Msg
showExtras links button =
    div [ class "nav-ctas" ]
        [ showLinks links
        , showButton button
        ]


showLinks : Maybe (List ( String, String )) -> Html Msg
showLinks maybe =
    case maybe of
        Nothing ->
            span [] []

        Just list ->
            span [] (List.map link list)


link : ( String, String ) -> Html Msg
link ( name, url ) =
    a [ href url, class "nav-item", attribute "data-letters" name ] [ text name ]


showButton : Maybe (Html Msg) -> Html Msg
showButton maybe =
    case maybe of
        Nothing ->
            span [] []

        Just button ->
            button


editor : Rules -> String -> Html Msg
editor model placeholder =
    Ace.toHtml
        [ Ace.onSourceChange RulesChanged
        , Ace.value model.text
        , Ace.theme "chrome"
        , Ace.highlightActiveLine False
        , Ace.useSoftTabs False
        , Ace.showPrintMargin False
        , Ace.placeholder placeholder
        ]
        []
