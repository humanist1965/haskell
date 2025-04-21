{-

In the N-Queens puzzle:
You have an n × n chessboard (e.g., 8 × 8 for a standard chessboard).

You must place n queens on the board such that no two queens attack each other:
Specifically, no queens can share the same row, column, or diagonal.

-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use (,)" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}

import Data.List (permutations)


{- QueensNxNBoard 

We will represent an NxN queens board by a List of integers
Example 8x8 board = [1,0,2,3,5,4,6,7]

(nth 0) = The postion of the queen for column 0
(nth 1) = The postion of the queen for column 1

Using this representation ensures that:
There can be no overall of rows and columns (1 of the key rules of the queens puzzle)
-}
type QueensNxNBoard = [BoardSize]
type BoardSize = Int -- 8 = standard board, but use bigger boards to make problem more challenging

--  Create a more readable way to extract an item from a list
nth :: [a] -> Int -> a
nth list pos = list !! pos

-- Type alias for Set
-- We will use addToSet to add new items and ensure there are no duplicates
type Set a = [a]

addToSet :: Eq a => a -> Set a -> Set a
addToSet it set =
    if it `elem` set
        then set
        else it : set


-- getAllNxNBoards return a list of all possible QueensNxNBoard(s) with unique (row, colum) positions for the queens
getAllNxNBoards :: BoardSize -> [QueensNxNBoard]
getAllNxNBoards boardSize = getAllPermutations [0..(boardSize - 1)]


getAllPermutations :: QueensNxNBoard -> [QueensNxNBoard]
getAllPermutations = permutations


-- filter out boards where queens are on the same diagonal
-- NOTE: There is a maths trick for doing this
filterSameDiagonal :: [QueensNxNBoard] -> [QueensNxNBoard]
filterSameDiagonal boardList =
    filter
        (\x -> noDiagonalOverlaps x)
        boardList

noDiagonalOverlaps :: QueensNxNBoard -> Bool
noDiagonalOverlaps board =
  let n = length board
      checkPair (col1, row1) (col2, row2) = abs (row1 - row2) /= abs (col1 - col2)
      queenPositions = zip [0..n-1] board -- [(column, row)]
  in and [checkPair q1 q2 |
            i <- [0 .. n - 1],
            let q1 = queenPositions !! i,
            j <- [i + 1 .. n - 1],
            let q2 = queenPositions !! j]

enumList :: [a] -> [(Int, b)]
enumList myList =
  zipWith (\index item -> (index, item)) [0..length myList - 1] myList


solveQueensNxN :: BoardSize -> [QueensNxNBoard]
solveQueensNxN boardSize = filterSameDiagonal (getAllNxNBoards boardSize)

