module Redirects.View exposing (render)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))
import Models exposing (Rules)
import Redirects.Dump exposing (stringRule)
import Redirects.Parser exposing (Response, ParseResult, Rule, filterRules)
import Partials
import Erl


render : Rules -> Html Msg
render model =
    let
        binding =
            { model = model
            , message = ParseRedirects
            , docsLink = "https://www.netlify.com/docs/redirects"
            , placeholder = redirectsPlaceholder
            , parse = parseRedirects
            }
    in
        Partials.renderPage binding


parseRedirects : Rules -> Html Msg
parseRedirects model =
    let
        response =
            filterRules model.updatedText
    in
        if List.isEmpty response.results then
            div [ class "results empty-data" ] []
        else
            Partials.renderResults (resultsHeader response) (resultsBody response) ParseRedirects


resultsBody : Response -> List (Html msg)
resultsBody response =
    if List.isEmpty response.errors then
        List.map renderSuccessRule response.results
    else
        List.map renderErrorRule response.errors


renderSuccessRule : ParseResult -> Html msg
renderSuccessRule parse =
    case parse.result of
        Err msg ->
            div [] []

        Ok rule ->
            let
                parts =
                    String.split "\n" (stringRule rule)

                divs =
                    List.map (\s -> div [] [ (text s) ]) parts
            in
                div [ class "parse-result structured" ] divs


renderErrorRule : ParseResult -> Html msg
renderErrorRule parse =
    case parse.result of
        Err msg ->
            div [ class "parse-result error" ]
                [ div [] [ text (parse.rule ++ " : " ++ msg) ]
                ]

        Ok rule ->
            div [] []


resultsHeader : Response -> Html msg
resultsHeader response =
    Partials.resultHeader "redirects" (List.length response.errors)


redirectsPlaceholder =
    """# Type here the redirect rules you want to check.
# You can use hashtags to write comments.
# Examples:

/books/grove-high-output-management.html      /books/grove/high-output-management      301    # Simple redirect
/books/grove-only-the-paranoid-survive.html   /books/grove/only-the-paranoid-survive   301!   # Redirect ignoring if path exists
/books/meadows-limits-to-growth.html          /books/meadows/limits-to-growth          302    # Found redirect
/books/meadows-thinking-in-systems.html       /books/meadows/thinking-in-systems       303    # See other redirect

/the-art-of-closing   https://blog.jessfraz.com/post/the-art-of-closing     200    # Proxy rule, browser location won't change, only available in PRO plans
/a-nerd-in-a-cave     http://randsinrepose.com/archives/a-nerd-in-a-cave/   200!   # Proxy rule, ignoring if path exists
"""
