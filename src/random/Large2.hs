{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module Large2 (main) where

import TimedAction(timedAction)

bigList :: [Integer] = [1..4426165368]


productRes = product bigList


out :: Integer -> IO ()
out n = do
  putStrLn $ "Result = " ++ (take 20 $ show n)


main :: IO ()
main = do
    timedAction ("Count bigList took") (out productRes )