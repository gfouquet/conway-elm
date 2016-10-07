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

    }

type alias Experiment = Array (Array Cell)
type alias Cell = Bool
type alias Row = Int
type alias Col = Int
type alias Coord = (Row, Col)

init : (Model, Cmd Msg)
init =
    let width = 5
    in
        ({ width = width
        , experiment = emptyCells width
        }, Cmd.none)

emptyCells : Int -> Experiment
emptyCells width =
    Array.initialize width (\r -> Array.initialize width (\c -> False))


-- UPDATE
type Msg =
    Resize String
    | ToggleCell Coord Bool

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case log "message" msg of
        Resize val ->
            let width = Result.withDefault 0 (toInt val)
            in ({ model | experiment = emptyCells width, width = width }, Cmd.none)
        ToggleCell coord cell ->
            ({ model | experiment = toggle model.experiment coord }, Cmd.none)

toggle : Experiment -> Coord -> Experiment
toggle experiment coord =
    let (row, col) = coord
        rcell = Maybe.withDefault Array.empty (Array.get row experiment)
        cell = Maybe.withDefault False (Array.get col rcell)
    in Array.set row (Array.set col (not cell) rcell) experiment


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

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
                , Attr.max "30"
                , value (toString model.width)
                , size 5
                , onInput Resize
                ] []
            , input [ type' "button", value "start"] []
            , input [ type' "button", value "stop"] []
            , input [ type' "button", value "random"] []
            ]
        , definitionTable model.experiment
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
                , Css.width (px 5)
                , Css.height (px 5)
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