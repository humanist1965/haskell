{- 

My First attempt to create a permutations algorithm in haskell.

Replaced with src/permutations2.hs: Which is slightly slower but more readable

-}


{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

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

cons :: a -> [a] -> [a]
cons it list = 
    it : list

inc_mod_check :: Int -> [Int] -> Int -> Int
inc_mod_check base usedInts n = let 
    newNum = (n+1) `mod` base
    usedAlready = newNum `elem` usedInts in
        if usedAlready then
            inc_mod_check base usedInts newNum
        else 
            newNum


makeList :: StartPos -> NumItems -> MaxPos -> UsedList -> OptionsList
makeList start numItems maxPos usedList =
    if numItems == 1 then
        [start]
    else
         cons start (makeList (inc_mod_check maxPos usedList start) (numItems-1) maxPos usedList)


genPerm :: StartPos -> NumItems -> MaxPos -> PartialPermList -> [PermList] -> [PermList]
genPerm start numItems maxPos partialList resList = 
    let 
        optionsList = makeList start numItems maxPos partialList
        permsList = foldl (\res num-> 
            let partialList2 = cons num partialList
                nextNum = inc_mod_check maxPos partialList2 start 
                nextSize = (numItems-1)
            in
                if nextSize == 0 then
                    cons partialList resList
                else
                    genPerm nextNum nextSize maxPos partialList2 res
            ) resList optionsList
        in permsList

perms :: NumItems ->  [PermList]
perms numItems  = genPerm 0 numItems numItems [] []


out :: [PermList] -> IO ()
out res = do
  putStrLn $ "Number perms = " ++ show (length res)


main :: IO ()
main = forM_ [8..11] $ \n -> do
    timedAction ("perms " ++ show n) (out $ perms n)
    timedAction ("permutations " ++ show n) (out $ permutations [0..n-1])

