module Utils exposing (..)
import List exposing (length,repeat,take,drop,map,foldr)

split : Int -> List a -> List (List a)
split i list =
  case take i list of
    [] -> []
    listHead -> listHead :: split i (drop i list)