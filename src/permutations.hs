
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

import Data.List (permutations)
import System.CPUTime
import Control.Monad (when)
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





-- Modified timedAction to take the action as a parameter
timedAction :: String -> IO a -> IO a
timedAction description action = do
  start_cpu <- getCPUTime
  start_real <- getCurrentTime -- Use getCurrentTime for wall clock time
  result <- action
  end_cpu <- getCPUTime
  end_real <- getCurrentTime
  let duration_cpu = (fromIntegral (end_cpu - start_cpu)) / (10^12) :: Double -- Convert to seconds
      duration_real = realToFrac (diffUTCTime end_real start_real) :: Double
  -- putStrLn $ description ++ " (CPU): " ++ show duration_cpu ++ " seconds"
  putStrLn $ description ++ " (Real): " ++ show duration_real ++ " seconds"
  return result

out :: [PermList] -> IO ()
out res = do
  putStrLn $ "Number perms = " ++ show (length res)


main :: IO ()
main = do
  timedAction "perms1 8" (out $ perms 8)
  timedAction "perms1 9" (out $ perms 9)
  timedAction "perms1 10" (out $ perms 10)
  timedAction "permutatiions 10" (out $ permutations [0..9])
  timedAction "perms1 11" (out $ perms 11)
  timedAction "permutatiions 11" (out $ permutations [0..10])
  
  
