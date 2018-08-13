module LogRecord.ParseRmsTimestamp where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Time
import Control.Monad
import LogRecord.Timestamp

{-
  RMS time stamps are of the form
    18-01-02_13:14:22.977
  meaning 2018, Jan 2, 13:14:22.977
-}


digitPair :: Parser Int
digitPair = do
  d1 <- digit
  d2 <- digit
  let digits = [d1,d2]
  return $ read digits

digitTriple :: Parser Int
digitTriple = do
  d1 <- digit
  d2 <- digit
  d3 <- digit
  let digits = [d1,d2,d3]
  return $ read digits

parseDate :: Parser Day
parseDate = do
  yy <- digitPair
  char '-'
  mm <- digitPair
  char '-'
  dd <- digitPair
  return $ fromGregorian (toInteger $ fromYear yy) mm dd

parseRmsTime :: Parser DiffTime
parseRmsTime = do
  hh <- digitPair
  char ':'
  mm <- digitPair
  char ':'
  ss <- digitPair
  char '.'
  millis <- digitTriple
  let milliseconds = millisToDiffTime millis
      hms = toDiffTime hh mm ss
      actual = hms + milliseconds
  return actual

parseRmsDateTime :: Parser UTCTime
parseRmsDateTime = do
  date <- parseDate
  char '_'
  time <- parseRmsTime
  return (UTCTime date time)
