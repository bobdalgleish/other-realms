module Browser.Context where

import LogRecord.Log
import System.IO
import Browser.Predicate
import Browser.LogFormatter

type LogFilter a = Predicate (Log a)

data PrettyPrinter a = PrettyPrinter (Log a -> String)

printLog :: LogContext a -> Log a -> String
printLog ctx = \log -> let (PrettyPrinter f) = prettyPrinter ctx
                       in f log

data LogContext a = LogContext { file :: FilePath                 -- |name of the log file
                               , logLines :: [Log a]              -- |contents of the log file
                               , filters :: LogFilter a           -- |current filters
                               , formatters :: [LogFormatter a]   -- |reformat the logs
                               , prettyPrinter :: PrettyPrinter a -- |pretty print the result
                               }

-- |make a context, given a file name
makeContext :: FilePath -> LogContext
makeContext f = LogContext { file = f,
                             logLines = [],
                             filters = Success,
                             formatters = [emptyFormatter],
                             prettyPrinter = PrettyPrinter showFields
                           }
