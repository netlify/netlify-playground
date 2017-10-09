module Url exposing (..)

import Regex exposing (contains, regex)


fullUrl : String -> Bool
fullUrl protocol =
    Regex.contains (regex "^https?") protocol


validPattern : String -> Bool
validPattern part =
    (contains (regex "^(/|https?://\\w)") part)


validStatus : String -> Bool
validStatus status =
    (contains (regex "^200|301|302|303|307|404!?$") status)
