
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}

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

         
removeItem :: Eq a => a -> [a] -> [a]
removeItem x xs = filter (/= x) xs

removeAllUpToItem :: Int -> [Int] -> [Int]
removeAllUpToItem x xs = foldl (\res num->removeItem num res) xs [0..x]

genPerm :: OptionsList -> NumItems -> PartialPermList -> [PermList] -> [PermList]
genPerm optionsList numItems partialList resList = 
    foldl (\res num-> 
            let partialList2 = num : partialList
                resLen = length partialList2
                nextOptionsList = 
                  if resLen == 1 then 
                    removeAllUpToItem num optionsList 
                  else removeItem num optionsList
            in
                if resLen == numItems then
                    partialList2 : resList
                else if null nextOptionsList then
                    resList
                else
                    genPerm nextOptionsList numItems partialList2 res
            ) resList optionsList

combinations :: NumItems -> NumItems ->  [PermList]
combinations m n  = genPerm [0..m-1] n [] []


out :: [PermList] -> IO ()
out res = do
  putStrLn $ "Number perms = " ++ show (length res)


main :: IO ()
main = forM_ [3..5] $ \n -> do
    timedAction ("combinations " ++ show n) (out $ combinations (n*n) n)
