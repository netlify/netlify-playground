module Partials exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Messages exposing (Msg)
import Models exposing (Binding, Rules)
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
                [ onClick (NewUrl "/redirects")
                , class "nav-item"
                , attribute "data-letters" "Redirects"
                ]
                [ text "Redirects" ]
            , a
                [ onClick (NewUrl "/headers")
                , class "nav-item"
                , attribute "data-letters" "Headers"
                ]
                [ text "Headers" ]
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


parseTestButton : Rules -> (String -> Msg) -> Html Msg
parseTestButton rules msg =
    Html.a
        [ class "nav-item smaller btn-primary nav-cta"
        , onClick (msg rules.text)
        ]
        [ text "Test rules" ]


renderPage : Binding -> Html Msg
renderPage binding =
    let
        model =
            binding.model

        button =
            Just (parseTestButton model binding.message)

        links =
            Just [ ( "Docs", binding.docsLink ) ]
    in
        div [ class "main" ]
            [ pageHeader model links button
            , main_
                []
                [ editor model binding.placeholder
                , binding.parse model
                ]
            ]


renderResults : Html Msg -> List (Html Msg) -> (String -> Msg) -> Html Msg
renderResults header errors message =
    div [ class "results" ]
        [ header
        , div [ class "results-list" ] errors
        , button
            [ type_ "button"
            , title "Close results panel"
            , onClick (message "")
            ]
            [ text "x" ]
        ]


resultHeader : String -> Int -> Html msg
resultHeader name count =
    if count > 0 then
        div [ class "results-header error" ]
            [ text ("Oh no! We found " ++ toString count ++ " errors") ]
    else
        div [ class "results-header success" ]
            [ text ("Yay! All " ++ name ++ " are valid") ]
