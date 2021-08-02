module Team exposing (..)

import Random exposing (Generator,constant,pair,map)
import Random.Float exposing (normal)
import Dict exposing (Dict,map)
import Utils exposing (split)
import Tuple exposing (pair)
import List exposing (map2)

--TO-DO, implement a parameterized mapping into this

--implement everything with float for now, in reality bools and ints would be allowed
--we can use this fancy type overloading to keep states encoded
type TeamAtrribute 
	--min max pair, useful to bundle in pair for generation
  = AttributeRange Tuple.pair(Float, Float)
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

--need to think more about how to form this
generateAttributes : Dict String (Generator TeamAtrribute) -> Generator( Dict String TeamAtrribute)
generateAttributes generator_dict =
	Random.map
		(\generator -> 
			Dict.map
				(\k v -> generator)
				generator_dict
			)

--eventually think about changing random gen-> create paradigm
generateTeam : Int -> String -> Generator (Dict String TeamAtrribute) -> Generator Team
generateTeam num team_name attr_generators =
	Random.map
		(\x -> Team num team_name x)
		attr_generators
--style guide notes at some point

--createTeam : Int -> String ->  List String -> List Tuple.pair(Float,Float) -> Team
--createTeam num team_name attr_names attr_min_maxes =
--	{ number : num
--	, name : team_name
--	, attrs : 
--		Dict.fromList
--			map2 
--				(\key val -> (key,val)) 
--				attr_names 
--				(map attr_min_maxes AttributeRange)
--	}