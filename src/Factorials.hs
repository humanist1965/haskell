{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use camelCase" #-}
module Factorials( factorial, combinations_count, main) where

import System.IO (hFlush, stdout)
import Control.Monad (forM_)

factorial :: Integer -> Integer

factorial 0 = 1
factorial n = n * factorial (n-1)


combinations_count:: Integer -> Integer -> Integer
combinations_count m n = (factorial m) `div` ((factorial n) * (factorial (m - n)))

main :: IO ()
main = do
    putStrLn ""
    putStrLn "Number of ways to place 8 queens on an 8x8 board:"
    print $ combinations_count 64 8
    hFlush stdout  -- Forces flush after each output line
