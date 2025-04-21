type StartPos = Int
type NumItems = Int 
type MaxPos = Int
type PartialPermList = [Int]
type PermList = [Int]
type OptionsList = [Int]

cons :: a -> [a] -> [a]
cons it list = 
    it : list

makeList :: StartPos -> NumItems -> MaxPos -> OptionsList
makeList start numItems maxPos =
    if numItems == 1 then
        [start]
    else
         cons start (makeList ((start+1) `mod` maxPos) (numItems-1) maxPos)

forEach :: StartPos -> NumItems -> MaxPos -> PartialPermList -> [PermList]
forEach start numItems maxPos partialList
    | numItems == 0 = [partialList]
    | otherwise = do
        num <- makeList start (maxPos - length partialList) maxPos
        forEach ((start + 1) `mod` maxPos) (numItems-1) maxPos (partialList ++ [num])