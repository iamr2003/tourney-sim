module Team exposing (..)

import Dict exposing (Dict, foldl, map, toList)
import List exposing (append, foldl, singleton)
import Random exposing (Generator, constant, map, pair)
import Random.Extra exposing (combine)
import Random.Float exposing (normal)
import Set exposing (Set, toList)
import Tuple exposing (first, second)
import Utils exposing (split)



--TO-DO, implement a parameterized mapping into this
--FIX MORE TYPE STUFF
--implement everything with float for now, in reality bools and ints would be allowed
--we can use this fancy type overloading to keep states encoded


type
    TeamAttribute
    --min max pair, useful to bundle in pair for generation
    = AttributeRange ( Float, Float )
    | AttributeValue Float --this might not be helpful here


generateAttribute : Generator ( Float, Float ) -> Generator TeamAttribute
generateAttribute boundGenerator =
    Random.map (\x -> AttributeRange x) boundGenerator



--might eventually make a wrapper of type Float | Generator Float to make these things easier


generateResult : TeamAttribute -> Generator TeamAttribute
generateResult attr =
    case attr of
        AttributeRange bounds ->
            Random.map
                (\n -> AttributeValue n)
                (let
                    min =
                        first bounds

                    max =
                        second bounds

                    mean =
                        (min + max) / 2

                    stdev =
                        (max - mean) / 3

                    --99.7% of distribution is between 3 stdevs of origin
                 in
                 Random.Float.normal mean stdev
                )

        AttributeValue val ->
            --safety, this should never be called in usage
            Random.map
                (\n -> AttributeValue n)
                (Random.constant val)



--this method is dumb, but need to think


coerceAttribute : TeamAttribute -> Float
coerceAttribute attr =
    case attr of
        AttributeRange range ->
            0

        --again a case that should never happen, so somethigns should be rearranged
        AttributeValue val ->
            val


type alias Team =
    { number : Int
    , name : String --optional field
    , attrs : Dict String TeamAttribute
    }



--written with the help of joelq on the elm slack
--combineDict : Dict String (Generator a) -> Generator (Dict String a)
--combineDict dict =
--    Dict.foldl
--        (\key valueGen comboGen ->
--            Random.map2 (Dict.insert key) valueGen comboGen
--        )
--        (Random.constant Dict.empty)
--        dict
--need to think more about how to form this


generateAttributes : Dict String (Generator TeamAttribute) -> Generator (Dict String TeamAttribute)
generateAttributes generator_dict =
    Dict.foldl
        (\key valueGen comboGen ->
            Random.map2 (Dict.insert key) valueGen comboGen
        )
        (Random.constant Dict.empty)
        generator_dict



--eventually think about changing random gen-> create paradigm


generateTeam : Int -> String -> Generator (Dict String TeamAttribute) -> Generator Team
generateTeam num team_name attr_generators =
    Random.map
        (\x -> Team num team_name x)
        attr_generators



--TYPES ARE STILL AN ISSUE here, as well as foldl stuff is odd
--hyper specific implementation, but fine for now


generateTeams : Set Int -> Generator (Dict String TeamAttribute) -> Generator (Dict Int Team)
generateTeams teamSet attr_generators =
    let
        teamList =
            Set.toList teamSet
    in
    Random.map
        -- (List (Int , Team) -> Dict Int Team) -> Generator (List(Int,Team)) -> Generator (Dict Int Team)
        Dict.fromList
        (combine
            (List.map
                --List Int -> List (Generator (Int,Team))
                (\n ->
                    Random.pair (Random.constant n) (generateTeam n ("Team " ++ String.fromInt n) attr_generators)
                )
                teamList
            )
        )
