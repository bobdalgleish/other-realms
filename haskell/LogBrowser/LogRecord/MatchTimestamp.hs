module LogRecord.MatchTimestamp where

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

matchingStrings :: Regex -> String -> Maybe ([ String])
matchingStrings p d = do
                        pmp <- matchOnceText p d
                        let (pre, m, post) = pmp
                        return (map fst $ tail $ elems m)

matchingInts :: [String] -> [Int]
matchingInts m = map iread m
                 where iread d = read d :: Int

-- |Construct a date from a list of ints
dtFragments :: [Int] -> Maybe UTCTime
dtFragments listOfInts
          | (length listOfInts) /= 7 = Nothing
          | otherwise = do
                            dateFragment <- dayFromPieces (take 3 listOfInts)
                            dayTime <- timeFromPieces (drop 3 listOfInts)
                            return (UTCTime dateFragment dayTime)
