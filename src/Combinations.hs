
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant if" #-}

module Combinations
  (  combinations, main
  ) where


import TimedAction(timedAction)
import Data.List (permutations)
import System.CPUTime
import Control.Monad (when, forM_)
import Data.Time.Clock (diffUTCTime, getCurrentTime) -- Import for a slightly different time


type NumItems = Int 
type PartialPermList = [Int]
type PermList = [Int]
type OptionsList = [Int]


afterItem :: Eq a => a -> [a] -> [a]
afterItem x xs = tail $ dropWhile (/= x) xs

genPerm :: OptionsList -> NumItems -> PartialPermList -> [PermList] -> [PermList]
genPerm optionsList numItems partialList resList = 
    foldl (\res num-> 
            let partialList2 = num : partialList
                resLen = length partialList2
                nextOptionsList = afterItem num optionsList
            in
                if resLen == numItems then
                    partialList2 : res
                else if null nextOptionsList then
                    res 
                else
                    genPerm nextOptionsList numItems partialList2 res
            ) resList optionsList

combinations :: NumItems -> NumItems ->  [PermList]
combinations m n  = genPerm [0..m-1] n [] []


out :: [PermList] -> IO ()
out res = do
  putStrLn $ "Number perms = " ++ show (length res)


main :: IO ()
main = forM_ [3..7] $ \n -> do
    timedAction ("combinations " ++ show n) (out $ combinations (n*n) n)
