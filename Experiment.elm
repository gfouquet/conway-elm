module Experiment exposing (Experiment, Cell, empty, random, toggle)

import Coord exposing (..)
import Array exposing (Array, fromList, toList)
import Random exposing (Seed)
import List exposing (..)
import Debug exposing (log)

type alias Experiment = Array (Array Cell)
type alias Cell = Bool

empty : Int -> Experiment
empty width =
    Array.initialize width (\r -> Array.initialize width (\c -> False))

random : Seed -> Int -> (Experiment, Seed)
random initSeed width =
    let
        pairs = foldl
            (\r acc -> case head acc of
                Nothing -> [rowSupplier width initSeed]
                Just (_, seed) -> rowSupplier width seed :: acc
            ) [] [1..width]

        newSeed = case head pairs of
            Nothing -> initSeed
            Just (_, seed) -> seed

    in
        (log "pairs" pairs |> map (\(row,_) -> fromList row) |> fromList
        , log "new seed" newSeed)

toggle : Experiment -> Coord -> Experiment
toggle experiment coord =
    let (row, col) = coord
        rcell = Maybe.withDefault Array.empty (Array.get row experiment)
        cell = Maybe.withDefault False (Array.get col rcell)
    in Array.set row (Array.set col (not cell) rcell) experiment

-- INTERNAL FUNCTIONS

rowSupplier width seed =
    Random.step (Random.list width Random.bool) seed