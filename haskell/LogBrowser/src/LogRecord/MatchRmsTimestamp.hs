module LogRecord.MatchRmsTimestamp where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import Text.Regex.Base.RegexLike
import Text.Regex.PCRE
import Data.List
import Data.Array.IArray
import LogRecord.Timestamp
import LogRecord.MatchTimestamp

rmsDateTime = shortDate "-" ++ "_" ++ timeMs ":"

datePattern = makeRegex rmsDateTime :: Regex


-- |Create a conversion function from a string to a date
dateTimeConverter :: Regex -> ([Int] -> Maybe UTCTime) -> String -> Maybe UTCTime
dateTimeConverter r c = \dateSource -> do
                                  stringMatches <- matchingStrings r dateSource
                                  c $ matchingInts stringMatches
                                  
-- |Convert to a date time using the above date pattern
convertToDateTime :: String -> Maybe UTCTime
convertToDateTime = dateTimeConverter datePattern dtFragments
