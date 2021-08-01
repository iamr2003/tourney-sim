module Team exposing (..)

import Random exposing (Generator,constant)
import Random.Float exposing (normal)
import Dict exposing (Dict)

--TO-DO, implement a parameterized mapping into this

--implement everything with float for now, in reality bools and ints would be allowed
--we can use this fancy type overloading to keep states encoded
type TeamAtrribute 
	--generator might eventually be smarter than just a min max
  = AttributeRange Float Float 
  | AttributeValue Float


--might eventually make a wrapper of type Float | Generator Float to make these things easier
generateAttribute : TeamAttribute -> Generator Float
generateAttribute attr =
	case attr of
		AttributeRange min max ->
			let 
				mean = (max+min)/2
				stdev = (max-mean)/3 --99.7% of distribution is between 3 stdevs of origin
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

