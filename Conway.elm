module Conway exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes as Attr
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toInt)
import Array exposing (Array, toList)
import Debug exposing (..)
import Css exposing (margin, padding, backgroundColor, height, width, px, rgb)
import Random exposing (Seed, initialSeed)
import Time exposing (Time, millisecond)
import Experiment exposing (Experiment, Cell)
import Coord exposing (..)
import Maybe exposing (Maybe)

main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL

type alias Model =
    { width : Int
    , experiment : Experiment
    , state : State
    , randomSeed : Seed
    }

type State = Running | Stopped

init : (Model, Cmd Msg)
init =
    let width = 5
    in
        ({ width = width
        , experiment = Experiment.empty width
        , state = Stopped
        -- random seed should be initialized with current time when I understand how Time.now works
        , randomSeed = initialSeed <| 1234
        }, Cmd.none)


-- UPDATE
type Msg =
    Resize String
    | ToggleCell Coord Bool
    | Clear
    | RandomExperiment
    | InitExperiment (List (List Bool))
    | Start
    | Stop
    | NextBatch Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case log "message" msg of
        Resize val ->
            let width = Result.withDefault 0 (toInt val)
            in ({ model | experiment = Experiment.empty width, width = width }, Cmd.none)
        ToggleCell coord cell ->
            ({ model | experiment = Experiment.toggle model.experiment coord }, Cmd.none)
        Clear -> ({ model | experiment = Experiment.empty model.width }, Cmd.none)
        RandomExperiment -> (model, Random.generate InitExperiment (Experiment.randomField model.width))
        InitExperiment field -> ({ model | experiment = Experiment.initField field}, Cmd.none)
        Start -> ({ model | state = Running }, Cmd.none)
        Stop -> ({ model | state = Stopped }, Cmd.none)
        NextBatch time -> ({ model | experiment = Experiment.next model.experiment}, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Stopped -> Sub.none
        Running -> Time.every (100 * millisecond) NextBatch

-- VIEW
styles =
    Css.asPairs >> Html.Attributes.style

view : Model -> Html Msg
view model =
    div[]
        [ Html.form []
            [ input
                [ type' "number"
                , Attr.min "0"
                , Attr.max "100"
                , value (toString model.width)
                , size 5
                , onInput Resize
                ] []
            , input [ type' "button", value "start", disabled (model.state == Running), onClick Start] []
            , input [ type' "button", value "stop", disabled (model.state == Stopped), onClick Stop ] []
            , input [ type' "button", value "random", disabled (model.state == Running), onClick RandomExperiment] []
            , input [ type' "button", value "clear", disabled (model.state == Running), onClick Clear] []
            ]
        , if model.state == Stopped then definitionTable model.experiment else div [] []
        , experimentPane model.experiment
        ]

definitionTable : Experiment -> Html Msg
definitionTable cells = (tableBuilder definitionCell) cells

definitionCell : Row -> Col -> Cell -> Html Msg
definitionCell row col cell =
    td [] [ input [ type' "checkbox", checked cell, onCheck (ToggleCell (row,col)) ] [] ]

experimentPane : Experiment -> Html Msg
experimentPane cells = (tableBuilder experimentCell) cells

experimentCell row col cell =
    let style cell =
        if cell then rgb 0 128 128 else rgb 255 255 255
    in
        td [ styles
                [ backgroundColor (style cell)
                , Css.width (px 3)
                , Css.height (px 3)
                ]
            ] [text " "]

tableBuilder : (Row -> Col -> Cell -> Html Msg) -> (Experiment -> Html Msg)
tableBuilder cellBuilder =
    \cells ->
        table [style [("border-collapse", "collapse")]] (cells |> Array.indexedMap (\row cs -> (rowBuilder cellBuilder) row cs) |> toList)

rowBuilder : (Row -> Col -> Cell -> Html Msg) -> (Row -> Array Cell -> Html Msg)
rowBuilder cellCell =
    \row cells ->
        tr [ styles [margin (px 0), padding (px 0)] ] (cells |> Array.indexedMap (\col cs -> cellCell row col cs) |> toList)