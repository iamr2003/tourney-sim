module AttributeGenerator exposing (..)

import Browser
import Design exposing (..)
import Dict exposing (..)
import Element exposing (..)
import Random exposing (Generator)
import Team exposing (..)


type alias DistFactors =
    { max : Float
    , min : Float
    , mean : Float
    }


type alias Model =
    { uiState : Dict String DistFactors
    }


rulesToFactors : Dict String Float -> Dict String DistFactors
rulesToFactors rules =
    Dict.map
        (\k v ->
            --arbitrary set up heuristic, could be adjusted to better defaults
            DistFactors 0 (5 * v) (10 * v)
        )
        rules


type Msg
    = UpdateMax String Float
    | UpdateMin String Float
    | UpdateMean String Float


init : Dict String Float -> Model
init rules =
    Model (rulesToFactors rules)


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
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

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
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

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
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


view : Model -> Element Msg
view model =
    Element.row
        []
        (Dict.values
            (Dict.map
                (\k v ->
                    --slider for every factor
                    numberSlider k v.min v.max (Just 1) v.mean (UpdateMean k)
                )
                model.uiState
            )
        )



--make generator thigns
