{- 

How Elegantly can your favourite programming language handle large Lists of data.

Challenge:

1. Can you setup a List with 4.3 billion integer items on it

2. Can you then process the data in the list:
    2.1 Sum - sum all items
    2.2 Product - multiple items

-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module LargeListChallenge(main ) where


import TimedAction(timedAction)
import Prelude hiding (product, sum)

type IntegerList = [Integer]

-- create a large List of integers
--bigList :: IntegerList = [1..4426165368]
bigList :: IntegerList = (createBigList 4426165368)

type Size = Integer
type Count = Integer
type ResultList = [Integer]
type ResultAcc = ResultList


createBigList :: Size ->   ResultList
createBigList size = createBigList' size 1 []

createBigList' :: Size -> Count -> ResultAcc ->  ResultList
createBigList' size count resAcc
  | count > size = resAcc
  | otherwise     = createBigList' size (count + 1) (count : resAcc) 




-- Let's make a reader friendly foreachLoop (higher-order) function

type InputType  a = a
type ResultType b = b
type InputList a = [InputType a]
type InitialResult b = b
type LoopFunc a b = (ResultType b -> InputType a -> ResultType b)

foreachLoop :: (LoopFunc a b ) -> ResultType b -> InputList a -> ResultType b

-- Our foreachLoop is just an alias onto haskell's foldl function (what other languages call a reduce function)
foreachLoop = foldl



sum :: [Integer] -> Integer
sum bigList = foreachLoop (\resAcc num -> resAcc+num) 0 bigList


product :: [Integer] -> Integer
product bigList = foreachLoop (\resAcc num -> resAcc*num) 1 bigList

count :: [Integer] -> Integer
count bigList = foreachLoop (\resAcc num -> resAcc+1) 0 bigList


out :: Integer -> IO ()
out n = do
  putStrLn $ "Result = " ++ (take 20 $ show n)


main :: IO ()
main = do
    timedAction ("Count bigList took") (out (count bigList) )
  


