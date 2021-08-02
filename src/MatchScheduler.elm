module MatchScheduler exposing (..)

--DEBUG ISSUE WITH SOMETIMES 2 TEAM ALLIANCES(surrogate needed)
--DEBUG SOME MORE ISSUES WITH DISTRIBUTIONS, same team against each other on alliances

-- I don't like the amount of imports needed, maybe reorganize dependencies at some point
import Random exposing (Generator)
import List exposing (length,repeat,take,drop,map,foldr)
import Random.List exposing(shuffle)
import Set exposing (Set,size,toList,fromList)
import Utils exposing (split)

type alias Match =
  { red : Set Int
  , blue : Set Int
  }

--generateSchedule : Int -> Set Int -> Generator MatchSchedule

--could alternatively make matches per team be the thing inputted
--generateSchedule matches teamList = 
--  let matchesPerTeam = (matches*6) // (size teamList) 
--  in


--generator and post thing separated methods
createSchedule : List Int -> List Match
createSchedule matchList = 
  map createMatch (split 6 matchList)

-- need to set fancier set bounds on this, random.list.choices might be helpful
teamScheduler : Int -> Set Int -> Generator (List Int)
teamScheduler matchesPerTeam teamList =
  shuffle (foldr (++) [] (repeat matchesPerTeam (toList teamList)))


createMatch : List Int -> Match
createMatch teams =
  let 
    redList = take 3 teams
    blueList = drop 3 teams
  in
  { red = fromList redList
  , blue = fromList blueList
  }


  ----generator type issues
--generateScheduleByTeam : Int -> Set Int -> Generator MatchSchedule
--generateScheduleByTeam matchesPerTeam teamList =
--  map generateMatch (split 6 (teamScheduler matchesPerTeam teamList))
