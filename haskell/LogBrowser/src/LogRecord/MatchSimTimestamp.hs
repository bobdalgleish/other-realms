module LogRecord.MatchSimTimestamp where

import Text.Regex.Base.RegexLike
import Text.Regex.PCRE
import LogRecord.MatchTimestamp

simDateTime = longDate "/" ++ " " ++ timeMs ":"

convertSimDateTime = dateTimeConverter (makeRegex simDateTime :: Regex) dtFragments
