module Utils exposing (..)

import List exposing (drop, foldr, length, map, repeat, take)


split : Int -> List a -> List (List a)
split i list =
    case take i list of
        [] ->
            []

        listHead ->
            listHead :: split i (drop i list)
