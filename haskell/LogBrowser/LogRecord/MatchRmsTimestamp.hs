module LogRecord.MatchRmsTimestamp where

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import Text.Regex.Base.RegexLike
import Text.Regex.PCRE
import Data.List
import Data.Array.IArray
import LogRecord.Timestamp

-- |Pair of digits
digitPair = "(\\d\\d)"
-- |Triplet of digits
digitTrip = "(\\d\\d\\d)"
-- |Four digits
digitQuad = "(\\d\\d\\d\\d)"

-- |Date with two digit year
shortDate :: String -> String
shortDate sep = intercalate "-" [digitPair, digitPair, digitPair]

-- |Date with four digit year
longDate :: String -> String
longDate sep = intercalate sep [digitQuad, digitPair, digitPair]

-- |Time with milliseconds
timeMs :: String -> String
timeMs sep = intercalate sep [digitPair, digitPair, digitPair ++ "\\." ++ digitTrip]

rmsDateTime = shortDate "-" ++ "_" ++ timeMs ":"

datePattern = makeRegex rmsDateTime :: Regex

matchingStrings :: Regex -> String -> Maybe ([ String])
matchingStrings p d = do
                        pmp <- matchOnceText p d
                        let (pre, m, post) = pmp
                        return (map fst $ tail $ elems m)

matchingInts :: [String] -> [Int]
matchingInts m = map iread m
                 where iread d = read d :: Int

dateFromPieces :: [Int] -> Maybe Day
dateFromPieces (year:month:day:[]) = fromGregorianValid (toInteger $ yearDigits year) month day
                                     where yearDigits yy
                                                    | yy < 70   = 2000 + yy
                                                    | yy < 100  = 1900 + yy
                                                    | otherwise = yy
dateFromPieces _ = Nothing


dtFragments :: [Int] -> Maybe UTCTime
dtFragments listOfInts
          | (length listOfInts) /= 7 = Nothing
          | otherwise = do
                            dateFragment <- dayFromPieces (take 3 listOfInts)
                            dayTime <- timeFromPieces (drop 3 listOfInts)
                            return (UTCTime dateFragment dayTime)

-- |Create a conversion function from a string to a date
dateTimeConverter :: Regex -> ([Int] -> Maybe UTCTime) -> String -> Maybe UTCTime
dateTimeConverter r c = \dateSource -> do
                                  stringMatches <- matchingStrings r dateSource
                                  c $ matchingInts stringMatches
                                  
-- |Convert to a date time using the above date pattern
convertToDateTime :: String -> Maybe UTCTime
convertToDateTime = dateTimeConverter datePattern dtFragments
