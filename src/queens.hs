{-

In the N-Queens puzzle:
You have an n × n chessboard (e.g., 8 × 8 for a standard chessboard).

You must place n queens on the board such that no two queens attack each other:
Specifically, no queens can share the same row, column, or diagonal.

-}

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


getAllNxNBoards :: BoardSize -> [QueensNxNBoard]
getAllNxNBoards boardSize = getAllPermutations [0..(boardSize - 1)]


getAllPermutations :: [a] -> [[a]]
getAllPermutations = permutations