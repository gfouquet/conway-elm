module Experiment exposing (Experiment, Cell, empty, random, toggle, next)

import Coord exposing (..)
import Array exposing (Array, fromList, toList)
import Array as A
import Random exposing (Seed)
import List as L
import Debug exposing (log)
import Maybe exposing (withDefault)

type alias Experiment = Array (Array Cell)
type alias Cell = Bool

empty : Int -> Experiment
empty width =
    A.initialize width (\r -> A.initialize width (\c -> False))


random : Seed -> Int -> (Experiment, Seed)
random initSeed width =
    let
        pairs = L.foldl
            (\r acc -> case L.head acc of
                Nothing -> [randomRow width initSeed]
                Just (_, seed) -> randomRow width seed :: acc
            ) [] [1..width]

        newSeed = case L.head pairs of
            Nothing -> initSeed
            Just (_, seed) -> seed

    in
        (log "pairs" pairs |> L.map (\(row,_) -> fromList row) |> fromList
        , log "new seed" newSeed)


toggle : Experiment -> Coord -> Experiment
toggle experiment coord =
    let (row, col) = coord
        rcell = withDefault A.empty (A.get row experiment)
        cell = withDefault False (A.get col rcell)
    in A.set row (A.set col (not cell) rcell) experiment


next : Experiment -> Experiment
next experiment =
    experiment |> A.indexedMap (\rdx row -> row |> A.indexedMap (\cdx cell -> nextCell experiment (rdx,cdx) cell))


-- INTERNAL FUNCTIONS

randomRow width seed =
    Random.step (Random.list width Random.bool) seed


offsets = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

rule cell liveNeighbors =
    case cell of
        False -> if liveNeighbors == 3 then True else False
        True -> if liveNeighbors == 2 || liveNeighbors == 3 then True else False

neighbor : Array Cell -> Int -> Int -> Cell
neighbor ar dx off =
    case A.get (dx + off) ar of
    Nothing -> False
    Just n -> n

offsetsToNeighbor : Experiment -> Coord -> Coord -> Cell
offsetsToNeighbor experiment (rdx, cdx) (x, y) =
    case A.get (rdx + x) experiment of
    Nothing -> False
    Just row -> neighbor row cdx y

nextCell : Experiment -> Coord -> Cell -> Cell
nextCell experiment coord cell =
    offsets
    |> L.map (offsetsToNeighbor experiment coord)
    |> L.foldl (\c acc -> acc + if c then 1 else 0) 0
    |> rule cell
