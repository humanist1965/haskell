{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use (,)" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Queens0(solveQueensNxN, main ) where


import TimedAction(timedAction)
import Data.List (permutations)
import Control.Monad (forM_)
import Combinations(combinations)




type QueensNxNBoard = [QueenPosition] -- QueenPosition in {0,...n*n}
type NumberBoardSpaces = Int -- 64 for a standard board
type QueenPosition = Int -- 0..63 for a standard board
type NumberQueens = Int -- 8 for a standard board
type QueensBoardColRow = [ColRow] -- Explicit representation of the nxn queens board
type ColRow = (QueenColPosition,QueenRowPosition)
type QueenColPosition = Int -- 0..7 for a standard board
type QueenRowPosition = Int -- 0..7 for a standard board
type QueenDiagNormalPosition = Int 
type BoardSize = Int -- 8 = standard board, but use bigger boards to make problem more challenging

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


-- filter out boards where queens are on the same diagonal
filterSameDiagonal :: [QueensNxNBoard] -> [QueensNxNBoard]
filterSameDiagonal boardList =
    filter
        (\x -> noDiagonalOverlaps x)
        boardList


getAllCombinations :: NumberBoardSpaces -> NumberQueens -> [QueenPosition]
getAllCombinations = combinations

-- getAllNxNBoards return a list of all possible QueensNxNBoard(s) 
getAllNxNBoards :: BoardSize -> [QueensNxNBoard]
getAllNxNBoards boardSize = getAllCombinations (boardSize * boardSize) boardSize


solveQueensNxN :: BoardSize -> [QueensNxNBoard]
solveQueensNxN boardSize = filterSameDiagonal (getAllNxNBoards boardSize)

out :: Int -> IO ()
out n = do
  putStrLn $ "Number Solutions = " ++ show n


main :: IO ()
main = forM_ [8..11] $ \n -> do
    timedAction ("Queens Problem (nxn), for n = " ++ show n) (out $ length $ solveQueensNxN n)
