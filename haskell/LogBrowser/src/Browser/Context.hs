module Browser.Context where

import LogRecord.Log
import System.IO
import Browser.Predicate
import Browser.LogFormatter

type LogFilter = Predicate (Log String)

data PrettyPrinter = PrettyPrinter (Log String -> String)

printLog :: LogContext -> Log String -> String
printLog ctx = \log -> let (PrettyPrinter f) = prettyPrinter ctx
                       in f log

data LogContext = LogContext { file :: FilePath                      -- |name of the log file
                             , logLines :: [Log String]              -- |contents of the log file
                             , filters :: LogFilter                  -- |current filters
                             , formatters :: [LogFormatter String]   -- |reformat the logs
                             , prettyPrinter :: PrettyPrinter        -- |pretty print the result
                             }

-- |make a context, given a file name
makeContext :: FilePath -> LogContext
makeContext f = LogContext { file = f,
                             logLines = [],
                             filters = Success,
                             formatters = [emptyFormatter],
                             prettyPrinter = PrettyPrinter showFields
                           }
