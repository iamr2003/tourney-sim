module Main exposing (..)


import Browser
import Html exposing (Html, button, div, text,li)
import Html.Events exposing (onClick)
import List exposing (map,foldr)
import Random exposing (generate,Generator)
import Set exposing (Set,empty,toList)
import Random.Set exposing(set)
import MatchScheduler exposing (..)
import Team exposing (..)

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
  , teams : Set Int --maybe come up with an eventual bounding type
  , schedule : List Match
  }


init : () -> (Model, Cmd Msg)
init _ =
  ({ page = 0
   , teams = Set.empty
   , schedule = []
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

randomTeam : Generator Int
randomTeam =
  (Random.int 1 9999)

generateTeams : Int -> Generator(Set Int)
generateTeams n =
  set n randomTeam



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
      , generate MakeList (generateTeams 30)
      )
    MakeList list ->
      ( {model | teams = list }
        , Cmd.none
        )
    NewSchedule ->
      (model
      , generate MakeSchedule (teamScheduler 3 model.teams)
      )
    MakeSchedule scheduled ->
      ( {model | schedule = (createSchedule scheduled)}
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
  div [] (map rowItem (map String.fromInt list))

--I want to put these view functions somewhere else later
matchScheduleDisplay : List Match -> Html Msg
matchScheduleDisplay schedule =
  div [] (map matchDisplay schedule)

--there should be a better way to do this
padInt : Int -> String
padInt int = (String.fromInt int) ++ " "

--issue with sometimes incomplete matches
--eventually reformat with other operators for better syntax
matchDisplay : Match -> Html Msg
matchDisplay match =
  div []
  [ text ("red: " ++ (foldr (++) "" (map padInt (toList match.red))))
  , text ("blue: " ++ (foldr (++) "" (map padInt (toList match.blue))))
  ]

view : Model -> Html Msg
view model =
  div []
  [ button [ onClick Increment ] [ text "+" ]
  , div [] [ text (String.fromInt model.page) ]
  , button [ onClick Decrement ] [ text "-" ]
  , page model.page
  , button [ onClick NewList ] [ text "New Team List" ]
  , listDisplay (Set.toList model.teams)
  , button [ onClick NewSchedule ] [ text "New Schedule" ]
  , matchScheduleDisplay (model.schedule)
  ]