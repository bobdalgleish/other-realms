module LogRecord.MatchSimTimestamp where

import Text.Regex.Base.RegexLike
import Text.Regex.PCRE
import LogRecord.MatchRmsTimestamp

simDateTime = longDate "/" ++ " " ++ timeMs ":"

convertSimToDateTime = dateTimeConverter (makeRegex simDateTime :: Regex) dtFragments
