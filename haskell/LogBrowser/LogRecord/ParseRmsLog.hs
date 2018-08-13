module LogRecord.ParseRmsLog where

import Text.ParserCombinators.Parsec hiding (spaces)
import LogRecord.Log
import LogRecord.ParseRmsTimestamp

{-
 Log format looks like

18-01-02_13:14:22.977:ORMS:main| body text |method:Module.java:131:INFO

-}

parseLogRecord :: Parser (Log String)
parseLogRecord = do
  dt <- parseRmsDateTime
  char ':'
  app <- many1 (noneOf ":")
  char ':'
  thr <- many1 (noneOf "|")
  char '|'
  bdy <- many1 (noneOf "|")
  char '|'
  mth <- many1 (noneOf ":")
  char ':'
  mod <- many1 (noneOf ":")
  char ':'
  lno <- many1 (noneOf ":")
  char ':'
  lvl <- many1 (oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  return Log {
    timestamp = Timestamp dt,
    source = Source "RMS1",
    application = Application app,
    thread = Thread thr,
    body = Body bdy,
    methodName = MethodName mth,
    moduleName = ModuleName mod,
    moduleLineNo = MethodLineNumber lno,
    logLevel = toLogLevel lvl
    }
