module List.Extra exposing (..)

-- List.Extra

{-| Take elements in order as long as the predicate evaluates to `True`
-}
takeWhile : (a -> Bool) -> List a -> List a
takeWhile predicate list =
  case list of
    []      -> []
    x::xs   -> if (predicate x) then x :: takeWhile predicate xs
               else []


{-| Drop the given prefix from the list. If the list doesn't start with that prefix, return `Nothing`.
    stripPrefix [1,2] [1,2,3,4] == Just [3,4]
    stripPrefix [1,2,3] [1,2,3,4,5] == Just [4,5]
    stripPrefix [1,2,3] [1,2,3] == Just []
    stripPrefix [1,2,3] [1,2] == Nothing
    stripPrefix [3,2,1] [1,2,3,4,5] == Nothing
-}
stripPrefix : List a -> List a -> Maybe (List a)
stripPrefix prefix xs =
  let
    step e m =
      case m of
        Nothing -> Nothing
        Just [] -> Nothing
        Just (x::xs') -> if e == x then Just xs' else Nothing
  in
    List.foldl step (Just xs) prefix
