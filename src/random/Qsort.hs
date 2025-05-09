{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Qsort
  (   main
  ) where

import TimedAction(timedAction)
import Data.Time.Clock (diffUTCTime, getCurrentTime) -- Import for a slightly different time


qsort:: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y <= x] ++ [x] ++ qsort [y | y <- xs, y > x]


qsort2:: Ord a => [a] -> [a]

qsort2 []     = []
qsort2 (x:xs) =
  (qsort y) ++ [x] ++ (qsort z) where
      y = [y | y <- xs, y <= x]
      z = [y | y <- xs, y > x]



bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort (oldList) = 
  -- Decide whether we need to loop through the list again
  if newList == oldList 
    then newList 
    else bubbleSort newList 
  where
    x:y:xs = oldList
    newList = if x <= y 
      then x:bubbleSort (y:xs) 
      else y:bubbleSort (x:xs)




out :: [Int] -> IO ()
out res = do
  putStrLn $ "List = " ++ show res


list1 = [10,9,3,6,2,3]

main :: IO ()
main = do
    timedAction "Qsort " (out $ qsort list1)
