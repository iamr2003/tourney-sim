module Scoring exposing (..)

import Team exposing (..)
import Dict exposing (Dict,map,merge,empty)
import MatchScheduler exposing (Match)

--might put these in different files at some point

--this signature is kind of weird, not sure if I like the overloading
runMatch : Match -> Generator (Match)
runMatch match =
--TODO

runTeam : Team -> Generator (Team)
runTeam team =
  Random.map
    (\n -> Team team.number team.name n)
    (generateAttributes 
      (
        Dict.map generateResult team.attrs
      )
    )

scoreAttributes : Dict String TeamAttribute -> Dict String Float -> Dict String Float
scoreAttributes attr rules =
  scoreDict 
    (Dict.map 
      (\k v -> coerceAttribute v)
      attr
    )
    rules

--multiply type merge of dictionaries
scoreDict : Dict String Float -> Dict String Float -> Dict String Float
scoreDict dict rules =
  Dict.merge 
     (\key a -> Dict.insert key a) --figure out how to nullify this case
     (\key a b -> Dict.insert key (a * b))
     (\key b -> Dict.insert key b)
      dict
      rules
      Dict.empty