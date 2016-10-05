module Conway exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes as Attr
import Html.Attributes exposing (..)

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

type alias Experiment = List (List Cell)
type alias Cell = Bool

init : (Model, Cmd Msg)
init =
    let width = 5
    in
        ({ width = width
        , experiment = emptyCells width
        }, Cmd.none)

emptyCells : Int -> Experiment
emptyCells width =
    [1..width] |> List.map (\r -> [1..width] |> List.map (\c -> False))


-- UPDATE
type Msg
    = None

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)

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
    table [] (List.indexedMap (\row cs -> definitionRow row cs) cells)

definitionRow : Int -> List Cell -> Html Msg
definitionRow row cells =
    tr [] (List.indexedMap (\col cs -> definitionCell row col cs) cells)

definitionCell : Int -> Int -> Cell -> Html Msg
definitionCell row col cell =
    td [] [ input [ type' "checkbox", checked cell ] [] ]