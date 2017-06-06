module Headers.View exposing (render)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Messages exposing (Msg(..))
import Models exposing (Rules)
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
        if List.length response.rules == 0 then
            div [ class "results empty-data" ] []
        else
            Partials.renderResults (resultHeader response) (List.map renderErrorRules response.rules) ParseHeaders


renderErrorRules : Rule -> Html msg
renderErrorRules rule =
    if List.length rule.invalid > 0 then
        div [ class "parse-result error" ]
            [ div []
                [ p [] [ text (rule.path ++ " :") ]
                , ul [] (List.map (\s -> li [] [ text s ]) rule.invalid)
                ]
            ]
    else
        div [] []


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
