module AttributeGenerator exposing (..)

import Browser
import Dict exposing (..)
import Random exposing (Generator)
import Team exposing (..)


type alias DistFactors =
    { max : Int
    , min : Int
    , mean : Int
    }


type alias Model =
    { uiState : Dict String DistFactors
    }


type Msg
    = UpdateMax String Float
    | UpdateMin String Float
    | UpdateMean String Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        --probably can split into fns, reptition is dumb
        UpdateMax attr val ->
            case get attr model.uiState of
                Just factors ->
                    ( { model
                        | uiState =
                            insert
                                attr
                                { factors | max = val }
                                model.uiState
                      }
                    , Cmd.None
                    )

                Nothing ->
                    ( model, Cmd.None )

        UpdateMin attr val ->
            case get attr model.uiState of
                Just factors ->
                    ( { model
                        | uiState =
                            insert
                                attr
                                { factors | min = val }
                                model.uiState
                      }
                    , Cmd.None
                    )

                Nothing ->
                    ( model, Cmd.None )

        UpdateMean attr val ->
            case get attr model.uiState of
                Just factors ->
                    ( { model
                        | uiState =
                            insert
                                attr
                                { factors | mean = val }
                                model.uiState
                      }
                    , Cmd.None
                    )

                Nothing ->
                    ( model, Cmd.None )

view : Model -> Element Msg
view model =
    map
        (\k v ->
            --slider for every factor

            )

--make generator thigns