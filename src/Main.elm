module Main exposing (..)


import Browser
import Html exposing (Html, button, div, text,li)
import Html.Events exposing (onClick)
import List exposing (map,foldr)
import Random exposing (generate,Generator,pair)
import Set exposing (Set,empty,toList)
import Dict exposing(Dict,empty,fromList,map)
import Random.Set exposing(set)
import MatchScheduler exposing (..)
import Team exposing (..)
import Scoring exposing (..)

--PARAMS

--might make this more tree structure at some point(auto, Dict of Dict, some other structure), and better wrappers
--based on python simulator
--weekOneRanges = {
--  'auto':{'avgCrossLine':[0,1],'avgLowerBallsScored':[0,3],'avgOuterBallsScored':[0,3],'avgInnerBallsScored':[0,2],'avgMissedBalls':[0,3]},
--  'teleop':{'avgLowerBallsScored':[0,3],'avgOuterBallsScored':[0,3],'avgInnerBallsScored':[0,2],'avgMissedBalls':[0,3],'avgControlPanelRot':[0,1],'avgControlPanelPos':[0,1]},
--  'endgame':{'avgClimbState':[0,2],'avgBalanced':[0,1]}
--  #dont worry about other subjective stats yet 
--  }

boundPair : Float -> Float -> Generator (Float,Float)
boundPair min max =
  Random.pair (Random.constant min) (Random.constant max)

palmettoDist : Generator (Dict String TeamAttribute)
palmettoDist =
  generateAttributes
    (Dict.map
      (\key val -> generateAttribute val)
    --filled in only some things, can come up with full list later
      (Dict.fromList
        [ ("autoCrossLine",boundPair 0 1)
        , ("autoBalls",boundPair 0 6)
        , ("teleopBalls",boundPair 0 9)
        , ("climbState", boundPair 0 2) -- again, change into actual boolean/int at some point, or create rectification in rules
        , ("climbBalance", boundPair 0 1)
        ]
      )
    )

--these aren't the real rules, also again need to support more complex types
powerUpRules : Dict String Float
powerUpRules =
  Dict.fromList
      [ ("autoCrossLine",5)
      , ("autoBalls",4)
      , ("teleopBalls",2)
      , ("climbState", 15)
      , ("climbBalance", 15)
      ]




-- MAIN


main =
  Browser.element 
  { init = init
  , update = update
  , subscriptions = subscriptions 
  , view = view }


-- MODEL

type alias Model = 
  { page : Int 
  , teamNumbers : Set Int --maybe come up with an eventual bounding type
  , schedule : List Match
  , teams : Dict Int Team
  , attr_generators : Generator (Dict String TeamAttribute)
  }



init : () -> (Model, Cmd Msg)
init _ =
  ({ page = 0
   , teamNumbers = Set.empty
   , schedule = []
   , teams = Dict.empty
   , attr_generators = palmettoDist
   }
   , Cmd.none)



-- UPDATE

type Msg
  = Increment
  | Decrement
  | NewList
  | MakeList (Set Int)
  | NewSchedule
  | MakeSchedule (List Int)
  | NewTeams
  | MakeTeams (Dict Int Team)

randomTeamNumber : Generator Int
randomTeamNumber =
  (Random.int 1 9999)

generateTeamNumbers : Int -> Generator(Set Int)
generateTeamNumbers n =
  set n randomTeamNumber

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Increment ->
      -- figure out more condensed way to write this, probably can sub function if I want
     ( {model | page = model.page + 1}
       , Cmd.none
       )
    Decrement ->
      ({model | page = model.page - 1}
       , Cmd.none
       )
    NewList ->
      (model
      , generate MakeList (generateTeamNumbers 30)
      )
    MakeList list ->
      ( {model | teamNumbers = list }
        , Cmd.none
        )
    NewSchedule ->
      (model
      , generate MakeSchedule (teamScheduler 3 model.teamNumbers)
      )
    MakeSchedule scheduled ->
      ( {model | schedule = (createSchedule scheduled)}
        , Cmd.none
        )
    NewTeams ->
      ( model
      , generate MakeTeams (generateTeams model.teamNumbers model.attr_generators )
      )
    MakeTeams teamsMade ->
      ( {model | teams = teamsMade}
        , Cmd.none
        )


--SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW
page : Int -> Html Msg
page number =
  div []
    [text ("Page "++ String.fromInt number) ]


--swap with HTML map at some point
--intLine : Int -> String
--intLine int =
--  "\n"++ String.fromInt int

rowItem : String -> Html Msg
rowItem id =
    div []
        [ text id ]

listDisplay : List Int -> Html Msg
listDisplay list =
  div [] (List.map rowItem (List.map String.fromInt list))

--I want to put these view functions somewhere else later
matchScheduleDisplay : List Match -> Html Msg
matchScheduleDisplay schedule =
  div [] (List.map matchDisplay schedule)

--there should be a better way to do this
padInt : Int -> String
padInt int = (String.fromInt int) ++ " "

--issue with sometimes incomplete matches
--eventually reformat with other operators for better syntax
matchDisplay : Match -> Html Msg
matchDisplay match =
  div []
  [ text ("red: " ++ (foldr (++) "" (List.map padInt (toList match.red))))
  , text ("blue: " ++ (foldr (++) "" (List.map padInt (toList match.blue))))
  ]

view : Model -> Html Msg
view model =
  div []
  [ button [ onClick Increment ] [ text "+" ]
  , div [] [ text (String.fromInt model.page) ]
  , button [ onClick Decrement ] [ text "-" ]
  , page model.page
  , button [ onClick NewList ] [ text "New Team List" ]
  , listDisplay (Set.toList model.teamNumbers)
  , button [ onClick NewSchedule ] [ text "New Schedule" ]
  , matchScheduleDisplay (model.schedule)
  ]