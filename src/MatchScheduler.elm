module MatchScheduler exposing (..)

--DEBUG ISSUE WITH SOMETIMES 2 TEAM ALLIANCES(surrogate needed)
--DEBUG SOME MORE ISSUES WITH DISTRIBUTIONS, same team against each other on alliances
-- I don't like the amount of imports needed, maybe reorganize dependencies at some point

import List exposing (drop, foldr, length, map, repeat, take)
import Random exposing (Generator, andThen, map)
import Random.List exposing (shuffle)
import Set exposing (Set, empty, fromList, size, toList)
import Utils exposing (split)


type alias Match =
    { red : Set Int
    , blue : Set Int
    , surrogates : Set Int
    }



-- I think repetition issues might arise from this


createMatch : List Int -> Match
createMatch teams =
    let
        redList =
            take 3 teams

        blueList =
            drop 3 teams
    in
    { red = fromList redList
    , blue = fromList blueList
    , surrogates = Set.empty
    }



--unused, could eventually be put back into chain
--put of a mess since originally not written well


generateMatch : Generator (List Int) -> Generator Match
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
    List.foldr
        -- Generator (List Int)
        (\comboGen nextGen ->
            Random.map2
                (\x y -> x ++ y)
                comboGen
                nextGen
        )
        (Random.constant [])
        (List.map shuffle (repeat matchesPerTeam (toList teamList)))



-- List ( Generator List Int)
--schedule without surrogates


generatePureSchedule : Int -> Set Int -> Generator (List Match)
generatePureSchedule matchesPerTeam teamList =
    Random.map
        createSchedule
        (teamScheduler matchesPerTeam teamList)



--schedule with surrogates
--generateScheduleWithSurrogates : Generator (List Match) -> Generator (List Match)
--generateScheduleWithSurrogates pureSchedule =
--  Random.map
---- need to figure out how to make this a generator again, not a generator(generator situation)
--addSurrogatesInAlliance : List Int -> Set Int -> Set Int
--addInSurrogates teamList alliance =
--  let
--    teamsToAdd = 3 - (size alliance) --not putting in type safety for negative things, shouldn't be possible from existent, but need to check
--    addList = take teamsToAdd teamList
--  in
----generator type issues
--generateScheduleByTeam : Int -> Set Int -> Generator MatchSchedule
--generateScheduleByTeam matchesPerTeam teamList =
--  map generateMatch (split 6 (teamScheduler matchesPerTeam teamList))
