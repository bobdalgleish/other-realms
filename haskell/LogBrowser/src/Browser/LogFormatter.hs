module Browser.LogFormatter where

import LogRecord.Log
import Browser.Predicate

-- |Format a log record if the predicate matches
data LogFormatter a = LogFormatter (Predicate (Log a), (Log a -> Log a))

-- |Reformat the contents of a log record
applyFormatter :: LogFormatter a -> Log a -> Log a
applyFormatter (LogFormatter (p, f)) l = if applyPredicate p l then f l else l

-- |Don't reformat
emptyFormatter :: LogFormatter a
emptyFormatter = LogFormatter (Fail, id)
