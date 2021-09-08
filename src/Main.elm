module Main exposing (..)

--elm-ui imports

import Browser
import Dict exposing (Dict, empty, fromList, keys, map, values)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Events exposing (onClick)
import List exposing (foldr, map, take)
import MatchScheduler exposing (..)
import Random exposing (Generator, float, generate, pair)
import Random.Set exposing (set)
import Round exposing (round)
import Scoring exposing (..)
import Set exposing (Set, empty, toList)
import Team exposing (..)
import Tuple exposing (first, second)



--PARAMS
--might make this more tree structure at some point(auto, Dict of Dict, some other structure), and better wrappers
--based on python simulator
--weekOneRanges = {
--  'auto':{'avgCrossLine':[0,1],'avgLowerBallsScored':[0,3],'avgOuterBallsScored':[0,3],'avgInnerBallsScored':[0,2],'avgMissedBalls':[0,3]},
--  'teleop':{'avgLowerBallsScored':[0,3],'avgOuterBallsScored':[0,3],'avgInnerBallsScored':[0,2],'avgMissedBalls':[0,3],'avgControlPanelRot':[0,1],'avgControlPanelPos':[0,1]},
--  'endgame':{'avgClimbState':[0,2],'avgBalanced':[0,1]}
--  #dont worry about other subjective stats yet
--  }


boundPair : Float -> Float -> Generator ( Float, Float )
boundPair min max =
    Random.map
        --enforce ordering
        (\n ->
            if first n > second n then
                ( second n, first n )

            else
                n
        )
        (Random.pair
            (Random.float min max)
            (Random.float min max)
        )


palmettoDist : Generator (Dict String TeamAttribute)
palmettoDist =
    generateAttributes
        (Dict.map
            (\key val -> generateAttribute val)
            --filled in only some things, can come up with full list later
            (Dict.fromList
                [ ( "autoCrossLine", boundPair 0 1 )
                , ( "autoBalls", boundPair 0 6 )
                , ( "teleopBalls", boundPair 0 9 )
                , ( "climbState", boundPair 0 2 ) -- again, change into actual boolean/int at some point, or create rectification in rules
                , ( "climbBalance", boundPair 0 1 )
                ]
            )
        )



--these aren't the real rules, also again need to support more complex types


powerUpRules : Dict String Float
powerUpRules =
    Dict.fromList
        [ ( "autoCrossLine", 5 )
        , ( "autoBalls", 4 )
        , ( "teleopBalls", 2 )
        , ( "climbState", 15 )
        , ( "climbBalance", 15 )
        ]



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { page : Int
    , teamNumbers : Set Int --maybe come up with an eventual bounding type
    , schedule : List Match
    , results : List MatchResult
    , teams : Dict Int Team
    , attr_generators : Generator (Dict String TeamAttribute)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { page = 0
      , teamNumbers = Set.empty
      , schedule = []
      , results = []
      , teams = Dict.empty
      , attr_generators = palmettoDist
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NewList
    | MakeList (Set Int)
    | NewSchedule
    | MakeSchedule (List Match)
    | NewTeams
    | MakeTeams (Dict Int Team)
    | NewResults
    | MakeResults (List MatchResult)


randomTeamNumber : Generator Int
randomTeamNumber =
    Random.int 1 9999


generateTeamNumbers : Int -> Generator (Set Int)
generateTeamNumbers n =
    set n randomTeamNumber


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewList ->
            ( model
            , generate MakeList (generateTeamNumbers 30)
              --current numbers are perfect so no surrogates, fix later
            )

        MakeList list ->
            ( { model | teamNumbers = list }
            , Cmd.none
            )

        NewSchedule ->
            ( model
            , generate MakeSchedule (generatePureSchedule 3 model.teamNumbers)
            )

        MakeSchedule scheduled ->
            ( { model | schedule = scheduled }
            , Cmd.none
            )

        NewTeams ->
            --extraneous, but useful to be explicit
            ( model
              --intermediate level of generation needed
            , generate MakeTeams (generateTeams model.teamNumbers model.attr_generators)
            )

        MakeTeams teamsMade ->
            ( { model | teams = teamsMade }
            , Cmd.none
            )

        NewResults ->
            ( model
            , generate MakeResults (runMatches model.schedule model.teams)
            )

        MakeResults newResults ->
            ( { model | results = newResults }
            , Cmd.none
            )



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


viewMatchSchedule : List Match -> Element Msg
viewMatchSchedule schedule =
    Element.column [] (List.map viewMatch schedule)


viewAllianceNumbers : Set Int -> Element Msg
viewAllianceNumbers alliance =
    Element.row
        [ spacing 10
        ]
        (List.map
            (\n ->
                el
                    [ width (px 100)
                    , Font.center
                    ]
                    (text (String.fromInt n))
            )
            (toList alliance)
        )


viewMatch : Match -> Element Msg
viewMatch match =
    Element.column
        [ padding 5 ]
        [ el
            [ Background.color (rgb255 255 238 238) ]
            (viewAllianceNumbers match.red)
        , el
            [ Background.color (rgb255 238 238 255) ]
            (viewAllianceNumbers match.blue)
        ]



--I'm using (\n ->text (String.fromInt n)) a lot, should probably make it a real function at some point


viewTeamAttribute : String -> TeamAttribute -> Element Msg
viewTeamAttribute name attr =
    el []
        (Element.text
            (case attr of
                AttributeRange range ->
                    name ++ ": " ++ Round.round 2 (first range) ++ " " ++ Round.round 2 (second range)

                AttributeValue val ->
                    name ++ ": " ++ Round.round 2 val
            )
        )


viewTeam : Team -> Element Msg
viewTeam team =
    Element.column
        [ Font.size 14
        , Border.width 2
        , Border.rounded 4
        ]
        [ el [] (Element.text (String.fromInt team.number))
        , el [] (Element.text team.name)
        , Element.wrappedRow []
            (Dict.values
                (Dict.map
                    (\k v -> viewTeamAttribute k v)
                    team.attrs
                )
            )
        ]


viewAlliance : List Team -> Element Msg
viewAlliance alliance =
    Element.column
        []
        (List.map
            viewTeam
            alliance
        )


viewMatchResult : MatchResult -> Element Msg
viewMatchResult result =
    Element.column
        []
        [ viewAlliance result.red
        , viewAlliance result.blue
        , Element.row [] (List.map (\n -> text (String.fromInt n)) (Set.toList result.surrogates))
        ]


viewTeamListMaker : Model -> Element Msg
viewTeamListMaker model =
    Element.column
        [ spacing 5 ]
        [ Input.button [] { onPress = Just NewList, label = el [ width (px 150), Font.center ] (text "New Team List") }
        , Element.column
            [ centerX, spacing 5 ]
            (List.map
                (\n -> text (String.fromInt n))
                (Set.toList model.teamNumbers)
            )
        ]



--eventually integrate the results maker in here


viewMatchScheduleMaker : Model -> Element Msg
viewMatchScheduleMaker model =
    Element.column
        [ centerX ]
        [ Input.button [] { onPress = Just NewSchedule, label = el [ width (px 330), Font.center ] (text "New Match Schedule") }
        , viewMatchSchedule model.schedule
        ]


viewTeamMaker : Model -> Element Msg
viewTeamMaker model =
    Element.column
        []
        [ Input.button [] { onPress = Just NewTeams, label = text "New Team Properties" }
        , Element.wrappedRow
            [ spacing 5 ]
            (List.map
                viewTeam
                (Dict.values model.teams)
            )
        ]



-- make prettier


view : Model -> Html Msg
view model =
    Element.layout
        []
    <|
        Element.column
            []
            [ Element.wrappedRow
                [ spacing 20
                , alignTop
                ]
                [ viewTeamListMaker model
                , viewMatchScheduleMaker model
                ]
            , viewTeamMaker model
            ]



--div []
--[ button [ onClick NewList] [ text "New Team List" ] --need to figure out chaining later
--, viewList (Set.toList model.teamNumbers)
--, button [ onClick NewTeams] [ text "New Teams" ]
--, div [] --some weird display stuff
--  (List.map
--    teamDisplay
--    (Dict.values model.teams)
--  )
--, button [ onClick NewSchedule ] [ text "New Schedule" ]
--, viewMatchSchedule (model.schedule)
--, button [ onClick NewResults ] [ text "Run Matches" ]
--, div [] --some more display stuff should be done, especially in related to scoring
--  (List.map
--    viewMatchResult
--    (model.results)
--  )
--]
