module LogRecord.MatchSimLog where

import LogRecord.Timestamp
import LogRecord.Log
import LogRecord.MatchSimTimestamp
import LogRecord.Matchers

-- This defines a mapping from lines to logs. The match function can
-- be factored out
matchSimLogs :: Maybe (Log String) -> [String] -> [Log String]
matchSimLogs Nothing [] = []
matchSimLogs (Just l) [] = [l]
matchSimLogs ls (ln:lns) = case matchRecord ls ln of
                             (l1:l2:[]) -> l1 : (matchSimLogs (Just l2) lns)
                             (l1:[]) -> matchSimLogs (Just l1) lns

matchRecord :: (Maybe (Log String)) -> String -> [Log String]
matchRecord prior line =
  case convertSimDateTime line of
    Just ts -> let parts = splitBy ('\t'==) line
                   log = Log { timestamp = Timestamp ts,
                               source = Source "Sim",
                               application = Application (parts !! 2),
                               thread = Thread "na",
                               body = Body (parts !! 4),
                               methodName = MethodName (parts !! 3),
                               moduleName = ModuleName "na",
                               moduleLineNo = MethodLineNumber "na",
                               logLevel = LogInfo
                             }
               in case prior of
                    Just priorLog -> [priorLog, log]
                    Nothing -> [log]
