module Scoring exposing (..)

import Team exposing (Team, TeamAttribute,coerceAttribute)
import Dict exposing (Dict,map,merge,empty)



scoreAttributes : Dict String TeamAttribute -> Dict String Float -> Dict String Float
scoreAttributes attr rules =
  scoreDict (Dict.map attr coerceAttribute) rules

--multiply type merge of dictionaries
scoreDict : Dict String Float -> Dict String Float -> Dict String Float
scoreDict dict rules =
  Dict.merge 
     (\key a -> Dict.insert key a) --figure out how to nullify this case
     (\key a b -> Dict.insert key (a * b))
     (\key b -> Dict.insert key b)
      attr
      rules
      Dict.empty