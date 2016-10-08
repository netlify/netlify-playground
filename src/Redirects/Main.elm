module Redirects.Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onSubmit, onInput)
import String
import List
import Redirects.Parser exposing (Rule, filterRules)
import Erl


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { rules : String
    , updatedRules : String
    }


model : Model
model =
    Model "" ""



-- UPDATE


type Msg
    = Parse String
    | RulesChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RulesChanged newRules ->
            { model | rules = newRules } ! []

        Parse newRules ->
            { model | updatedRules = newRules } ! []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Html.form [ onSubmit (Parse model.rules) ]
            [ textarea
                [ style [ ( "width", "100%" ) ]
                , rows 10
                , placeholder "Redirect rules"
                , onInput RulesChanged
                ]
                []
            , button [ type' "submit" ] [ text "Parse" ]
            ]
        , parse model
        ]


parse : Model -> Html msg
parse model =
    let
        rules =
            filterRules model.updatedRules
                |> List.map renderRule
    in
        div [] rules


renderRule : Result String Rule -> Html msg
renderRule rule =
    case rule of
        Err msg ->
            div [ style [ ( "color", "red" ) ] ]
                [ div [] [ text msg ]
                ]

        Ok rule ->
            div [ style [ ( "color", "green" ) ] ]
                [ div [] [ text ("origin: " ++ (Erl.toString rule.origin)) ]
                ]
