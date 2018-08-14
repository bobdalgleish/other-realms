module LogRecord.MatchRmsLog where

import LogRecord.Log
import LogRecord.Timestamp
import LogRecord.MatchRmsTimestamp
import Data.List (break)

matchRecord :: (Maybe (Log String)) -> String -> [Log String]
matchRecord prior line =
  case convertToDateTime line of
    Just ts -> let partial = matchApplication (Log { timestamp = Timestamp ts, source = Source "RMS1" })  (drop 22 line)
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

-- |match the application, thread fields and start parsing the body
matchApplication :: Log String -> String -> Log String
matchApplication log line =
  let (app, appr) = break (==':') line
      (thr, thrr) = break (=='|') (tail appr)
  in matchBody (log {application = Application app, thread = Thread thr}) (tail thrr)

-- |match the body portion, followed by the postamble
matchBody :: Log String -> String -> Log String
matchBody log line =
  let allBody = line
      parts = splitBy ('|'==) allBody
      (bdy, meths) = breakEnd ('|'==) allBody
      (mth:mod:lno:lvl:[]) = splitBy (':'==) meths
  in log { body = Body bdy,
           methodName = MethodName mth,
           moduleName = ModuleName mod,
           moduleLineNo = MethodLineNumber lno,
           logLevel = toLogLevel lvl
         }

getPostAmble :: String -> Maybe (String, String, String, String)
getPostAmble post =
  let parts = splitBy (':'==) post
  if length parts != 4 then Nothing else let (mth: mod: lno: lvl: []) = parts in Just (mth, mod, lno, lvl) 

-- |Split on the last matching character
breakEnd :: (Char -> Bool) -> String -> (String, String)
breakEnd c s = let parts = splitBy c s
                   len   = length parts
                   back  = last parts
                   front = if len > 1 then (take (length s - 1 - length back) s) else []
               in (front, back)

-- |Split on the matching character
splitBy :: (Char -> Bool) -> String -> [String]
splitBy _ [] = []
splitBy p s = let (front, back) = break p s
              in front : if length back > 1 then (splitBy p (tail back)) else []
