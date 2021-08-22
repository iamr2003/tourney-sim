module Scoring exposing (..)

import Team exposing (..)
import List exposing (map,filtermap)
import Dict exposing (Dict,map,merge,empty,get)
import Set exposing (toList)
import Random.Extra exposing (combine)
import MatchScheduler exposing (Match)

--might put these in different files at some point

--I don't like the naming that much, has to switch from numbers to refs, might restructure at some point
type alias MatchResult =
  { red : List Team
  , blue : List Team
  , surrogates : Set Int
  }

runMatches

runMatch : Match -> Dict Int Team -> Generator (MatchResult)
runMatch match teams =
  Random.map2
    (\r b -> MatchResult r b match.surrogates)
    (runAlliance match.red teams)
    (runAlliance match.blue teams)

runAlliance : Set Int -> Dict Int Team -> Generator (List Team)
runAlliance alliance teams =
  let
    allianceList = toList alliance
  in
  combine
    (List.map
      runTeam
      (List.filtermap
        (\n -> get n teams)
        allianceList
      )
    )
--this signature is kind of weird, not sure if I like the overloading
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