{-

In the N-Queens puzzle:
You have an n × n chessboard (e.g., 8 × 8 for a standard chessboard).

You must place n queens on the board such that no two queens attack each other:
Specifically, no queens can share the same row, column, or diagonal.


NOTES:
- This brute-force algorithm can solve the queens problem for n <= 10
- For n > 10 it takes too long. Running it on n==11 now...

-}

-- Ignoring the VsCode haskell package hints for the following
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use (,)" #-}

module Queens2(solveQueensNxN, main ) where


import TimedAction(timedAction)
import Permutations3 (perms_with_filter, alwaysTrueFilter)
import Control.Monad (forM_)



{- QueensNxNBoard 

We will represent an NxN queens board by a List of integers
Example 8x8 board = [1,0,2,3,5,4,6,7]

(nth 0) = The postion of the queen for column 0
(nth 1) = The postion of the queen for column 1

Using this representation ensures that:
There can be no overall of rows and columns (1 of the key rules of the queens puzzle)
-}
type QueensNxNBoard = [QueenRowPosition] -- QueenColPosition implicit from position in List
type QueensBoardColRow = [ColRow] -- Explicit representation of the nxn queens board
type ColRow = (QueenColPosition,QueenRowPosition)
type QueenColPosition = Int -- 0..7 for a standard board
type QueenRowPosition = Int -- 0..7 for a standard board
type QueenDiagNormalPosition = Int -- Trick for determining if queens are on the same diagonals
type BoardSize = Int -- 8 = standard board, but use bigger boards to make problem more challenging

-- Type alias for Set
-- We will use addToSet to add new items and ensure there are no duplicates
type Set a = [a]

addToSet :: Eq a => a -> Set a -> Set a
addToSet it set =
    if it `elem` set
        then set
        else it : set



enumList :: [a] -> [(Int, a)]
enumList myList =
  zipWith (\index item -> (index, item)) [0..length myList - 1] myList

-- convert the terse QueensNxNBoard into a QueensBoardColRow
-- Needed for our noDiagonalOverlaps function
getRowColBoard :: QueensNxNBoard -> QueensBoardColRow
getRowColBoard board = enumList board

-- Given a QueensNxNBoard this function return True if the queens do not sit on the same diagonal
noDiagonalOverlaps :: QueensNxNBoard -> Bool
noDiagonalOverlaps board =
  let n = length board
      colRowBoard :: QueensBoardColRow = getRowColBoard board
      -- To check for diagonal attacks, we calculate two sets:
      -- 1. `diags1Set`: Stores the differences (row - column) for each queen. If any two queens are on the same "top-left to bottom-right" diagonal, they will have the same (row - column) value, and the set's length will be less than n.
      -- 2. `diags2Set`: Stores the sums (row + column) for each queen. If any two queens are on the same "top-right to bottom-left" diagonal, they will have the same (row + column) value, and the set's length will be less than n.
      diags1Set :: Set QueenDiagNormalPosition = foldl (\res (col,row)->addToSet (row-col) res) [] colRowBoard
      diags2Set :: Set QueenDiagNormalPosition = foldl (\res (col,row)->addToSet (row+col) res) [] colRowBoard
  in length diags1Set == n && length diags2Set == n



solveQueensNxN :: BoardSize -> [QueensNxNBoard]
solveQueensNxN boardSize = perms_with_filter noDiagonalOverlaps boardSize 

out :: Int -> IO ()
out n = do
  putStrLn $ "Number Solutions = " ++ show n


main :: IO ()
main = forM_ [8..11] $ \n -> do
    timedAction ("Queens Problem (nxn), for n = " ++ show n) (out $ length $ solveQueensNxN n)
