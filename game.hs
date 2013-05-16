module Game
( mapLookup
, findNeighbors
, shouldLive
, findNextCells
, nextStep
) where

import qualified Data.Map as Map
import qualified Data.Set as Set

mapLookup x y board =
    case Map.lookup (x, y) board of
        Just value -> value
        Nothing -> 0

findNeighbors x y board = do
    sum $ 
        drop 1 [mapLookup (x + diffX) (y + diffY) board
            | diffX <- [0,1,-1], diffY <- [0,1,-1]]

aliveCellShouldLive x y board
    | 2 <= neighbors && neighbors <= 3 = 1
    | otherwise = 0
    where neighbors = findNeighbors x y board

deadCellShouldLive x y board
    | neighbors == 3 = 1
    | otherwise = 0
    where neighbors = findNeighbors x y board

shouldLive x y board
    | alive == 1 = aliveCellShouldLive x y board
    | alive == 0 = deadCellShouldLive x y board
    where alive = mapLookup x y board

findNextCells board =
    Set.fromList [(x + diffX, y + diffY) 
        | (x, y) <- Map.keys board, diffX <- [-1..1], diffY <- [-1..1]]

nextStep board = do
    let nextCells = findNextCells board
    Map.fromList $ [((x, y), 1)
        | (x, y) <- Set.toList nextCells, (shouldLive x y board == 1)]
