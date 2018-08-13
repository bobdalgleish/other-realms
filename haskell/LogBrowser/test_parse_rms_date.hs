import LogRecord.Log
import LogRecord.ParseRmsTimestamp
import Text.ParserCombinators.Parsec hiding (spaces)

readExpr :: String -> String
readExpr input = case parse parseDate "day" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found " ++ show val
    
main = do
  parseTest parseDate "18-01-02"

