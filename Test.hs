module Main where

import HUnit
import Game

import qualified Data.Map as Map
import qualified Data.Set as Set

board = Map.fromList [((1, 1), 1),
                      ((0, 0), 1),
                      ((0, 1), 1),
                      ((2, 0), 1),
                      ((2, 1), 1),
                      ((5, 5), 1)]


test1 = TestCase (assertEqual "Find Neighbors" 4 (findNeighbors 1 1 board))
test2 = TestCase (assertEqual "Should stay alive" 1 (shouldLive 0 0 board))
test3 = TestCase (assertEqual "Should come alive" 1 (shouldLive 0 1 board))
test4 = TestCase (assertEqual "death from overpopulation" 0 (shouldLive 1 1 board))
test5 = TestCase (assertEqual "death from underpopulation" 0 (shouldLive 5 5 board))

emptyishBoard = Map.fromList[((0, 0), 1)]
test6 = TestCase (assertEqual "can find all adjacent cells" 9 (Set.size $ findNextCells emptyishBoard))

nextBoard = nextStep board
test7 = TestCase (assertEqual "came alive" 1 (mapLookup 0 1 nextBoard))
test8 = TestCase (assertEqual "stayed alive" 1 (mapLookup 0 0 nextBoard))
test9 = TestCase (assertEqual "overpopulation" 0 (mapLookup 1 1 nextBoard))
test10 = TestCase (assertEqual "underpopulation" 0 (mapLookup 5 5 nextBoard))

tests = TestList [TestLabel "test1" test1,
                  TestLabel "test2" test2,
                  TestLabel "test3" test3,
                  TestLabel "test4" test4,
                  TestLabel "test5" test5,
                  TestLabel "test6" test6,
                  TestLabel "test7" test7,
                  TestLabel "test8" test8,
                  TestLabel "test9" test9,
                  TestLabel "test10" test10]

main = do runTestTT tests
