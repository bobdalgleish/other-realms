module LogRecord.MatchRmsLog where

import LogRecord.Log
import LogRecord.Timestamp
import LogRecord.MatchRmsTimestamp
import LogRecord.Matchers

matchRms :: Maybe (Log String) -> [String] -> [Log String]
matchRms Nothing [] = []
matchRms (Just l) [] = [l]
matchRms ls (ln:lns) = case matchRmsRecord ls ln of
                   (l1:l2:[]) -> l1 : (matchRms (Just l2) lns)
                   (l1:[])    -> matchRms (Just l1) lns
                     

matchRmsRecord :: (Maybe (Log String)) -> String -> [Log String]
matchRmsRecord prior line =
  case convertToDateTime line of
    Just ts -> let (app, appr) = break (':'==) (drop 22 line)
                   (thr, thrr) = break ('|'==) (tail appr)
                   partial = matchBody (newLog
                                        (Timestamp ts)
                                        (Source "RMS1")
                                        (Application app)
                                        (Thread thr)
                                        "Placeholder")  (tail thrr)
               in case prior of
                    Just priorLog -> [priorLog,partial]
                    Nothing -> [partial]
    Nothing -> case prior of
                 Just log -> [mergeBody log line]
                 Nothing  -> []

mergeBody :: Log String -> String -> Log String
mergeBody log line = let (Body bdy) = body log
                         newBody = bdy ++ "\n" ++ line
                     in log { body = Body newBody }

-- |match the body portion, followed by the postamble
matchBody :: Log String -> String -> Log String
matchBody log line =
  let parts = splitBy ('|'==) line
  in if length parts == 1
  then log { body = Body line }
  else case getPostAmble (last parts) of
         Just (mth, mod, lno, lvl) ->
           let len = length (last parts)
               bdy = take (length line - len - 1) line
           in log { body = Body bdy,
                    methodName = MethodName mth,
                    moduleName = ModuleName mod,
                    moduleLineNo = MethodLineNumber lno,
                    logLevel = toLogLevel lvl
                  }
         Nothing -> log { body = Body line }

getPostAmble :: String -> Maybe (String, String, String, String)
getPostAmble post = case splitBy (':'==) post of
                      (mth: mod: lno: lvl: []) -> Just (mth, mod, lno, lvl)
                      _ -> Nothing
