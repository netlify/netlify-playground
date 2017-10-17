module Headers.View exposing (render)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))
import Models exposing (Rules)
import Headers.Dump exposing (stringRule)
import Headers.Parser exposing (Response, Rule, filterRules)
import Partials


render : Rules -> Html Msg
render model =
    let
        binding =
            { model = model
            , message = ParseHeaders
            , docsLink = "https://www.netlify.com/docs/headers-and-basic-auth"
            , placeholder = headersPlaceholder
            , parse = parse
            }
    in
        Partials.renderPage binding


parse : Rules -> Html Msg
parse model =
    let
        response =
            filterRules model.updatedText
    in
        if List.isEmpty response.rules then
            div [ class "results empty-data" ] []
        else
            Partials.renderResults (resultHeader response) (resultsBody response) ParseHeaders


resultsBody : Response -> List (Html msg)
resultsBody response =
    let
        ( withoutErrors, withErrors ) =
            List.partition (\r -> List.isEmpty r.invalid) response.rules
    in
        if List.isEmpty withErrors then
            List.map renderSuccessRules withoutErrors
        else
            List.map renderErrorRules withErrors


renderSuccessRules : Rule -> Html msg
renderSuccessRules rule =
    let
        parts =
            String.split "\n" (stringRule rule)

        divs =
            List.map (\s -> div [] [ (text s) ]) parts
    in
        div [ class "parse-result structured" ] divs


renderErrorRules : Rule -> Html msg
renderErrorRules rule =
    div [ class "parse-result error" ]
        [ div []
            [ p [] [ text (rule.path ++ " :") ]
            , ul [] (List.map (\s -> li [] [ text s ]) rule.invalid)
            ]
        ]


resultHeader : Response -> Html msg
resultHeader response =
    let
        count =
            List.map (\r -> r.invalid) response.rules
                |> List.concat
                |> List.length
    in
        Partials.resultHeader "headers" count


headersPlaceholder =
    """# Type here the header rules you want to check.
# You can use hashtags to write comments.
# Examples:

/
  Link: &lt;/styles.css&gt; rel=preload as=stylesheet
  Link: &lt;/main.js&gt; rel=preload as=script
  Link: &lt;/image.jpg&gt; rel=preload as=image
/*
  X-XSS-Protection: 1; mode=block
  X-Frame-Option: SAMEORIGIN
/assets/*
  Cache-Control: public, max-age=360000
"""
