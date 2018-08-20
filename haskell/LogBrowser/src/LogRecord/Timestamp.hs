module LogRecord.Timestamp where

import Data.Time
import Data.Time.Format

-- |Time that the log record was created
data Timestamp = Timestamp UTCTime

showTimestamp :: Timestamp -> String
showTimestamp (Timestamp t) = fmtTime millisecondsFmt t

instance Show Timestamp where show = showTimestamp

-- |Convert a number to a year. Two digit years in the range 70 to 99 are in the 20th century
fromYear :: Int -> Int
fromYear y
  | y < 70    = 2000 + y
  | y < 100   = 1900 + y
  | otherwise = y

-- |Convert hours, minutes, seconds to DiffTime
toDiffTime :: Int -> Int -> Int -> DiffTime
toDiffTime hours minutes seconds = secondsToDiffTime $ toInteger (hours * 3600 + minutes * 60 + seconds)

-- |Convert milliseconds to DiffTime
millisToDiffTime :: Int -> DiffTime
millisToDiffTime milliseconds =
  picosecondsToDiffTime (toInteger milliseconds * 1000000000)

timeFromPieces :: [Int] -> Maybe DiffTime
timeFromPieces (hours:minutes:seconds:milliseconds:[]) =
  let millis = millisToDiffTime milliseconds
      hms = toDiffTime hours minutes seconds
  in Just (hms + millis)
timeFromPieces _ = Nothing

dayFromPieces :: [Int] -> Maybe Day
dayFromPieces (y:m:s:[]) =
  fromGregorianValid (toInteger $ fromYear y) m s
dayFromPieces _ = Nothing


millisecondsFmt = "%Y-%m-%d_%H:%M:%S%3Q"

fmtTime :: FormatTime t => String -> t -> String
fmtTime f time = formatTime defaultTimeLocale f time
