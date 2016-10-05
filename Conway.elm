module Conway exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes as Attr
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toInt)
import Array exposing (Array, toList)
import Debug exposing (..)

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
        , div [] []
        ]

definitionTable : Experiment -> Html Msg
definitionTable cells =
    table [] (cells |> Array.indexedMap (\row cs -> definitionRow row cs) |> toList)

definitionRow : Row -> Array Cell -> Html Msg
definitionRow row cells =
    tr [] (cells |> Array.indexedMap (\col cs -> definitionCell row col cs) |> toList)

definitionCell : Row -> Col -> Cell -> Html Msg
definitionCell row col cell =
    td [] [ input [ type' "checkbox", checked cell, onCheck (ToggleCell (row,col)) ] [] ]