module Msg exposing (Msg(..))

import Browser
import Dict exposing (Dict)
import MatchScheduler exposing (..)
import Scoring exposing (..)
import Set exposing (Set)
import Team exposing (..)
import Url


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



--I might reorg a lot, currently a bit weird
