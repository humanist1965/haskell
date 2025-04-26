{- 

In this 3rd attempt to create a permutations algorithm in haskell.

We are going to be extending the 2nd attempt by adding filtering support whilst creating permutations.
This will allow us to check the validity of a perm without fully generating it.Applicative

NOTE: It is the type of optimisation you need for the Queens problem. To allow you to extend brute-force solution searches
beyond n=11

-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}


module Permutations3
  ( perms_with_filter, perms, FilterFunc, main
  ) where


import TimedAction(timedAction)
import Data.List (permutations)
import System.CPUTime
import Control.Monad (when, forM_)
import Data.Time.Clock (diffUTCTime, getCurrentTime) -- Import for a slightly different time



type StartPos = Int
type NumItems = Int
type MaxPos = Int
type UsedList = [Int]
type PartialPermList = [Int]
type PermList = [Int]
type OptionsList = [Int]
type FilterFunc = (PartialPermList -> Bool)


removeItem :: Eq a => a -> [a] -> [a]
removeItem x xs = filter (/= x) xs

genPerm :: FilterFunc -> OptionsList -> MaxPos -> PartialPermList -> [PermList] -> [PermList]
genPerm filterFunc optionsList maxPos partialList resList =
    foldl (\res num->
            let partialList2 = num : partialList
                isValid = filterFunc partialList2
                nextOptionsList = (if isValid then removeItem num optionsList else optionsList)
            in
                if not isValid then
                    resList
                else if null nextOptionsList then
                    partialList2 : resList
                else
                    genPerm filterFunc nextOptionsList maxPos partialList2 res
            ) resList optionsList



perms :: NumItems  -> [PermList]
perms numItems = genPerm alwaysTrueFilter [0..numItems-1] numItems [] []

perms_with_filter :: FilterFunc -> NumItems  -> [PermList]
perms_with_filter filterFunc numItems = genPerm filterFunc [0..numItems-1] numItems [] []


out :: [PermList] -> IO ()
out res = do
  putStrLn $ "Number perms = " ++ show (length res)

alwaysTrueFilter :: FilterFunc
alwaysTrueFilter partialPerm = True

testFilter2 :: FilterFunc
testFilter2 partialPerm
  | partialPerm == [0,1,2] = False
  | partialPerm == [2,0,1] = False
  | otherwise              = True

main :: IO ()
main = forM_ [8..11] $ \n -> do
    timedAction ("perms " ++ show n) (out $ perms n)
    timedAction ("permutations " ++ show n) (out $ permutations [0..n-1])
