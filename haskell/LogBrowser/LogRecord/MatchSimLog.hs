module LogRecord.MatchSimLog where

import LogRecord.Timestamp
import LogRecord.Log
import LogRecord.MatchSimTimestamp
import Data.List (break)

matchRecord :: (Maybe (Log String)) -> String -> [Log String]
matchRecord prior line =
  case convertSimDateTime line of
    Just ts -> 
