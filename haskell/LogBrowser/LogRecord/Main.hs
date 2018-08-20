module LogRecord.Main where

import LogRecord.Log
import LogRecord.MatchRmsLog
import System.IO
import Data.List (isInfixOf)
import Data.Time

bodyContains :: Body String -> [String] -> Bool
bodyContains (Body b) searches = any (\s -> isInfixOf s b) searches

rmsSignposts :: Log String -> Bool
rmsSignposts log = bodyContains (body log)           
             ["RMS redundancy state change to RS_",
              "Starting up with arguments:",
              "Updating operational state from OS",
              "Startup delay value is "]

main = do
  h <- openFile "/home/dalgleish/Downloads/orms.out" ReadMode
  contents <- hGetContents h
  startTime <- getCurrentTime
  mapM (putStrLn . showFields) $ filter rmsSignposts (matchRms Nothing (lines contents))
  endTime <- getCurrentTime
  putStrLn ( (show startTime) ++ " " ++ (show endTime))
  hClose h
