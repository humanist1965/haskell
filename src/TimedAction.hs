module TimedAction
  ( timedAction
  ) where

import System.IO (hFlush, stdout)
import System.CPUTime (getCPUTime)
import Data.Time (getCurrentTime, diffUTCTime, UTCTime)
import Text.Printf (printf)

-- | Times the execution of an IO action, printing the wall-clock time (real time)
-- in seconds and returning the action's result.
--
-- The function measures both CPU time and real time but currently only prints
-- the real time (wall-clock time) in the format:
-- "<description> (Real): <seconds> seconds".
--
-- Example:
-- >>> timedAction "example" (return 42)
-- example (Real): 0.000123 seconds
-- 42
timedAction :: String -> IO a -> IO a
timedAction description action = do
  start_cpu <- getCPUTime
  start_real <- getCurrentTime
  result <- action
  end_cpu <- getCPUTime
  end_real <- getCurrentTime
  let duration_cpu = (fromIntegral (end_cpu - start_cpu)) / (10^12) :: Double -- Convert to seconds
      duration_real = realToFrac (diffUTCTime end_real start_real) :: Double
  -- putStrLn $ description ++ " (CPU): " ++ show duration_cpu ++ " seconds"
  printf "%s (Real): %.6f seconds\n" description duration_real
  hFlush stdout
  return result