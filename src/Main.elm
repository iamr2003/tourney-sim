module Main exposing (..)

import Array exposing (Array, fromList)
import Browser
import Browser.Navigation as Nav
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
import Update.Extra exposing (andThen)
import Url
import Url.Parser exposing ((</>), Parser, int, map, oneOf, s, string)



--BUG WITH ODD NUMBER OF TEAMS, NEED TO GO BACK THROUGH AND fix the surrogate stuff
--also at low team numbers is a problem, gaps occur
--teams can also play themselves which is problematics
------------------------------------
--SPEC for first release
--Attribute generator knobs
--variable sample size knobs(team number, match number)
--simple rule import(all double)
--DONE:breakdowns on separate redirect(or some other clever UI solution)
--Some type of analysis(rank or something else)
--Some kind of result export
------------------------
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


infRechargeRules : Dict String Float
infRechargeRules =
    Dict.fromList
        [ ( "autoCrossLine", 5 )
        , ( "autoBalls", 4 )
        , ( "teleopBalls", 2 )
        , ( "climbState", 15 )
        , ( "climbBalance", 15 )
        ]



-- MAIN
--need to switch to web app for link breakdown stuff


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { numTeams : Int
    , teamNumbers : Set Int --maybe come up with an eventual bounding type
    , schedule : List Match
    , results : List MatchResult
    , teams : Dict Int Team
    , attr_generators : Generator (Dict String TeamAttribute)
    , rules : Dict String Float
    , key : Nav.Key
    , url : Url.Url
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model 30 Set.empty [] [] Dict.empty palmettoDist infRechargeRules key url
    , Cmd.none
    )
        |> andThen update NewList



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
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | UpdateMatchNum Float


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
            , generate MakeList (generateTeamNumbers model.numTeams)
              --current numbers are perfect so no surrogates, fix later
            )

        -- some cool callback chaining, change things that it depends on
        MakeList list ->
            ( { model | teamNumbers = list }
            , Cmd.none
            )
                |> andThen update NewTeams
                |> andThen update NewSchedule

        NewSchedule ->
            ( model
            , generate MakeSchedule (generatePureSchedule 3 model.teamNumbers)
            )

        MakeSchedule scheduled ->
            ( { model | schedule = scheduled }
            , Cmd.none
            )
                |> andThen update NewResults

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
                |> andThen update NewResults

        NewResults ->
            ( model
            , generate MakeResults (runMatches model.schedule model.teams)
            )

        MakeResults newResults ->
            ( { model | results = newResults }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        UpdateMatchNum newNum ->
            ( { model
                | numTeams = floor newNum
              }
            , Cmd.none
            )
                |> andThen update NewList



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW
--these things can likely be reordered or broken into files, finding stuff is annoying atm


viewMatchSchedule : List Match -> Element Msg
viewMatchSchedule schedule =
    Element.column [] (List.map viewMatch schedule)


viewMatchResults : List MatchResult -> Dict String Float -> Element Msg
viewMatchResults results rules =
    Element.column [] (List.indexedMap (\i n -> viewMatchResult (i + 1) n rules) results)


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


viewMatchResult : Int -> MatchResult -> Dict String Float -> Element Msg
viewMatchResult match_number result rules =
    let
        redScore =
            scoreAlliance result.red rules

        blueScore =
            scoreAlliance result.blue rules
    in
    Element.column
        [ padding 5
        ]
        [ el
            [ Background.color (rgb255 255 238 238)
            , if redScore > blueScore then
                Font.bold

              else
                Font.regular
            ]
            (link [] { url = "/result/" ++ String.fromInt match_number, label = Element.text (Round.round 0 redScore) })
        , el
            [ Background.color (rgb255 238 238 255)
            , if redScore < blueScore then
                Font.bold

              else
                Font.regular
            ]
            (link [] { url = "/result/" ++ String.fromInt match_number, label = Element.text (Round.round 0 blueScore) })
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
        , Element.wrappedRow [ padding 2 ]
            (Dict.values
                (Dict.map
                    (\k v -> viewTeamAttribute k v)
                    team.attrs
                )
            )
        ]



--eh colors look a tad weird with background, I'll fix later


viewAlliance : List Team -> Color -> Element Msg
viewAlliance alliance color =
    Element.column
        [ Background.color color
        , spacing 1
        ]
        (List.map
            viewTeam
            alliance
        )



--can create some extra little boxes explaining overall alliance stuff


viewMatchBreakdown : Maybe MatchResult -> Element Msg
viewMatchBreakdown m_result =
    case m_result of
        Just result ->
            Element.column
                [ spacing 10 ]
                [ viewAlliance result.red (rgb255 255 238 238)
                , viewAlliance result.blue (rgb255 238 238 255)
                , Element.row [] (List.map (\n -> text (String.fromInt n)) (Set.toList result.surrogates))
                ]

        Nothing ->
            Element.none


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
    Element.row
        []
        [ Element.column
            [ centerX ]
            [ Input.button [] { onPress = Just NewSchedule, label = el [ width (px 330), Font.center ] (text "New Match Schedule") }
            , viewMatchSchedule model.schedule
            ]
        , Element.column
            [ centerX ]
            [ Input.button [] { onPress = Just NewResults, label = el [ width (px 150), Font.center ] (text "New Results") }
            , viewMatchResults model.results model.rules
            ]
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



--I'm making these like components, which may be bad
--can come up with a more rolling style later


viewNumTeamsSelector : Model -> Element Msg
viewNumTeamsSelector model =
    Input.slider
        [ Element.height (Element.px 30)

        -- Here is where we're creating/styling the "track", need to fix the following
        , Element.behindContent
            (Element.el
                [ Element.width Element.fill
                , Element.height (Element.px 2)
                , Element.centerY
                , Background.color (rgb255 128 128 128)
                , Border.rounded 2
                ]
                Element.none
            )
        ]
        { onChange = UpdateMatchNum
        , label =
            Input.labelAbove []
                (text "Number of Teams")
        , min = 0
        , max = 75
        , step = Just 6
        , value = toFloat model.numTeams
        , thumb =
            Input.defaultThumb
        }



--I know this is not the best organization for this, but will fix a bit later


type View_Mode
    = Main
    | Result_Breakdown Int



--this is all super jank, should use proper mapping parser stuff


parseURLtoView : Url.Url -> View_Mode
parseURLtoView url =
    let
        match_number =
            Url.Parser.parse (s "result" </> int) url
    in
    case match_number of
        Nothing ->
            Main

        Just n ->
            Result_Breakdown n


view : Model -> Browser.Document Msg
view model =
    let
        view_mode =
            parseURLtoView model.url
    in
    case view_mode of
        Main ->
            { title = "Main Page"
            , body =
                --body is supposed to have multiple nodes in a list, but idk how to switch between them
                List.singleton
                    (Element.layout
                        []
                     <|
                        Element.column
                            []
                            [ viewNumTeamsSelector model
                            , Element.wrappedRow
                                [ spacing 20
                                , alignTop
                                ]
                                [ viewTeamListMaker model
                                , viewMatchScheduleMaker model
                                ]
                            , viewTeamMaker model

                            --, link [] { url = "/result/1", label = text "A link" } --now ready to do fancy parsing
                            ]
                    )
            }

        Result_Breakdown n ->
            { title = "Match Breakdown"
            , body =
                List.singleton
                    (Element.layout
                        []
                        (Element.column
                            []
                            [ text ("Match " ++ String.fromInt n)

                            --this code is kind of stupid, I'll fix types up and down the line later
                            , viewMatchBreakdown (Array.get (n - 1) (Array.fromList model.results))
                            ]
                        )
                    )
            }
