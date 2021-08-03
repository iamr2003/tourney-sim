module Team exposing (..)

import Random exposing (Generator,constant,pair,map)
import Random.Float exposing (normal)
import Dict exposing (Dict,map,foldl,toList)
import Utils exposing (split)
import Tuple exposing (pair)
import List exposing (map2)

--TO-DO, implement a parameterized mapping into this

--implement everything with float for now, in reality bools and ints would be allowed
--we can use this fancy type overloading to keep states encoded
type TeamAtrribute 
	--min max pair, useful to bundle in pair for generation
  = AttributeRange (Float, Float)
  | AttributeValue Float --this might not be helpful here

generateAttribute : Generator (Float , Float) -> Generator TeamAtrribute
generateAttribute boundGenerator =
	Random.map (\x -> AttributeRange boundGenerator)

--might eventually make a wrapper of type Float | Generator Float to make these things easier
generateResult : TeamAttribute -> Generator Float
generateResult attr =
	case attr of
		AttributeRange bounds ->
			let 
				mean = (bounds.first+bounds.second)/2
				stdev = (bounds.second-mean)/3 --99.7% of distribution is between 3 stdevs of origin
			in
				(Random.Float.normal mean stdev)
		AttributeValue val -> --safety, this should never be called in usage
			Random.constant val

coerceAttribute : TeamAttribute -> Float
coerceAttribute attr =
	case attr of
		AttributeRange min max ->
			0 --again a case that should never happen, so somethigns should be rearranged
		AttributeValue val ->
			val

type alias Team =
	{ number : Int
	,	name : String --optional field
	,	attrs : Dict String TeamAtrribute
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
generateAttributes : Dict String (Generator TeamAtrribute) -> Generator( Dict String TeamAtrribute)
generateAttributes generator_dict =
    Dict.foldl
        (\key valueGen comboGen ->
            Random.map2 (Dict.insert key) valueGen comboGen
        )
        (Random.constant Dict.empty)
        generator_dict

--eventually think about changing random gen-> create paradigm
generateTeam : Int -> String -> Generator (Dict String TeamAtrribute) -> Generator Team
generateTeam num team_name attr_generators =
	Random.map
		(\x -> Team num team_name x)
		attr_generators

--hyper specific implementation, but fine for now
generateTeams : Set Int -> Generator (Dict String TeamAtrribute) -> Generator (Dict Int Team)
generateTeams teamSet attr_generators =
	let
		teamList = Dict.toList teamSet
	in
		Random.map
			Dict.fromList
			List.foldl --List (Generator Team) -> Generator (List Team)
				(\n comboGen ->
					Random.map 
						(List.append comboGen) 
						Random.pair
							n
							(generateTeam n ("Team" ++ String.fromInt(n)) attr_generators)
				)
				(Random.constant List.empty)
				teamList
