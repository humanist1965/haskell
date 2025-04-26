{- 

My 2nd attempt to create a permutations algorithm in haskell.

First atttempt src/permutations1.hs: Was slightly quicker but less readable
removeItem is what is making it slower...

-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}

module Permutations2
  (  perms, main
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

genPerm :: OptionsList -> PartialPermList -> [PermList] -> [PermList]
genPerm optionsList partialList resList = 
    foldl (\res num-> 
            let partialList2 = num : partialList
                nextOptionsList = removeItem num optionsList
            in
                if null nextOptionsList then
                    partialList2 : resList
                else
                    genPerm nextOptionsList partialList2 res
            ) resList optionsList

perms :: NumItems ->  [PermList]
perms numItems  = genPerm [0..numItems-1] [] []


out :: [PermList] -> IO ()
out res = do
  putStrLn $ "Number perms = " ++ show (length res)


main :: IO ()
main = forM_ [8..11] $ \n -> do
    timedAction ("perms " ++ show n) (out $ perms n)
    timedAction ("permutations " ++ show n) (out $ permutations [0..n-1])
