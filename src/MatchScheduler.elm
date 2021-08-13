module MatchScheduler exposing (..)

--DEBUG ISSUE WITH SOMETIMES 2 TEAM ALLIANCES(surrogate needed)
--DEBUG SOME MORE ISSUES WITH DISTRIBUTIONS, same team against each other on alliances

-- I don't like the amount of imports needed, maybe reorganize dependencies at some point
import Random exposing (Generator,map)
import List exposing (length,repeat,take,drop,map,foldr)
import Random.List exposing(shuffle)
import Set exposing (Set,size,toList,fromList)
import Utils exposing (split)

type alias Match =
  { red : Set Int
  , blue : Set Int
  , surrogates : Set Int
  }

createMatch : List Int -> Match
createMatch teams =
  let 
    redList = take 3 teams
    blueList = drop 3 teams
  in
  { red = fromList redList
  , blue = fromList blueList
  , surrogates = []
  }

--unused, could eventually be put back into chain
--put of a mess since originally not written well
generateMatch Generator (List Int): Generator Match
generateMatch teams =
  Random.map
    createMatch
    teams

--generator and post thing separated methods
createSchedule : List Int -> List Match
createSchedule matchList = 
  List.map createMatch (split 6 matchList)

-- need to set fancier set bounds on this, random.list.choices might be helpful
teamScheduler : Int -> Set Int -> Generator (List Int)
teamScheduler matchesPerTeam teamList =
  shuffle (foldr (++) [] (repeat matchesPerTeam (toList teamList)))

generateSchedule : Int -> Set Int -> Generator (List Match)
generateSchedule matchesPerTeam teamList =
  Random.map
    createSchedule
    (teamScheduler matchesPerTeam teamList)

-- need to figure out how to make this a generator again, not a generator(generator situation)
--addSurrogatesInAlliance : List Int -> Set Int -> Set Int
--addInSurrogates teamList alliance =
--  let 
--    teamsToAdd = 

  ----generator type issues
--generateScheduleByTeam : Int -> Set Int -> Generator MatchSchedule
--generateScheduleByTeam matchesPerTeam teamList =
--  map generateMatch (split 6 (teamScheduler matchesPerTeam teamList))
