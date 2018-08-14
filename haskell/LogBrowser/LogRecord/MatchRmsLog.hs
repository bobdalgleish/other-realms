module LogRecord.MatchRmsLog where

import LogRecord.Log
import LogRecord.Timestamp
import LogRecord.MatchRmsTimestamp
import Data.List (break)

matchRecord :: (Maybe (Log String)) -> String -> [Log String]
matchRecord prior line =
  case convertToDateTime line of
    Just ts -> let partial = matchApplication (Log { timestamp = Timestamp ts })  (drop 22 line)
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

matchApplication :: Log String -> String -> Log String
matchApplication log line =
  let (app, appr) = break (==':') line
      (thr, thrr) = break (=='|') (tail appr)
  in matchBody (log {application = Application app, thread = Thread thr}) (tail thrr)

matchBody :: Log String -> String -> Log String
matchBody log line =
  let (bdy, meths) = breakEnd '|' line
      (mth:mod:lno:lvl:[]) = splitBy (':'==) meths
  in log { body = Body bdy,
           methodName = MethodName mth,
           moduleName = ModuleName mod,
           moduleLineNo = MethodLineNumber lno,
           logLevel = toLogLevel lvl
         }
  where
    breakEnd c s = let p = splitBy (c==) s
                       l = length p
                       b = last p
                       f = if l > 1 then (take (1 + length b) s) else []
                   in (f, b)

splitBy :: (Char -> Bool) -> String -> [String]
splitBy _ [] = []
splitBy p s = let (front, back) = break p s
              in front : if length back > 1 then (splitBy p (tail back)) else []
