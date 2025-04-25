
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

import Data.List (permutations)


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
