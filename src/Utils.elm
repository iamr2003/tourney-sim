module Utils exposing (..)
split : Int -> List a -> List (List a)
split i list =
  case take i list of
    [] -> []
    listHead -> listHead :: split i (drop i list)