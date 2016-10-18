module Ace exposing (..)

{-| A library to use Ace editor with Elm.

# Editor
@docs toHtml

# Ace's Attributes
@docs theme, mode, value, highlightActiveLine, showPrintMargin, useSoftTabs, placeholder

# Ace's Events
@docs onSourceChange

-}

import Html exposing (Html, Attribute)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Encode as JE
import Json.Decode as JD
import Native.Ace


{-| Attribute to set the theme to Ace.

    Ace.toHtml [ Ace.theme "monokai" ] []
-}
theme : String -> Attribute msg
theme val =
    Attributes.property "AceTheme" (JE.string val)


{-| Attribute to set the mode to Ace.

    Ace.toHtml [ Ace.mode "lua" ] []
-}
mode : String -> Attribute msg
mode val =
    Attributes.property "AceMode" (JE.string val)


{-| Attribute to set initial value or to update current value of Ace.

    Ace.toHtml [ Ace.value "lua" ] []
-}
value : String -> Attribute msg
value val =
    Attributes.property "AceValue" (JE.string val)


{-| Attribute to set whether to show the print margin or not.

    Ace.toHtml [ Ace.showPrintMargin false ] []
-}
showPrintMargin : Bool -> Attribute msg
showPrintMargin val =
    Attributes.property "AceShowPrintMargin" (JE.bool val)


{-| Attribute to set whether to highlight the active line or not.

    Ace.toHtml [ Ace.highlightActiveLine false ] []
-}
highlightActiveLine : Bool -> Attribute msg
highlightActiveLine val =
    Attributes.property "AceHighlightActiveLine" (JE.bool val)


{-| Attribute to set whether to use soft tabs or not.

    Ace.toHtml [ Ace.useSoftTabs false ] []
-}
useSoftTabs : Bool -> Attribute msg
useSoftTabs val =
    Attributes.property "AceUseSoftTabs" (JE.bool val)


{-| Attribute to set a placeholder text inside the Ace editor.

    Ace.toHtml [ Ace.placeholder "write here" ] []
-}
placeholder : String -> Attribute msg
placeholder val =
    Attributes.property "AcePlaceholder" (JE.string val)


{-| Values changes listener. It used to get notifications about changes made by user.

    Ace.toHtml [ Ace.onSourceChange model.data ] []
-}
onSourceChange : (String -> msg) -> Attribute msg
onSourceChange tagger =
    Events.on "AceSourceChange" (JD.map tagger Events.targetValue)


{-| Creates `Html` instance with Ace attached to it.

    Ace.toHtml [] []
-}
toHtml : List (Attribute msg) -> List (Html msg) -> Html msg
toHtml =
    Native.Ace.toHtml
