module Countdown
  ( main
  ) where


import System.IO (hFlush, stdout)
import TimedAction(timedAction)
import System.CPUTime


choices :: [Int] -> [[Int]]
choices xs = [take i xs | i <- [1..(length xs - 1)]]

out :: [[Int]] -> IO ()
out res = do
  putStrLn $ "Choices = " ++ show res


main :: IO ()
main = do
    out $ choices [1,3,4,7, 25,75]
