import LogRecord.ParseRmsLog
import Text.ParserCombinators.Parsec

main = parseTest parseLogRecord "18-01-02_13:14:22.977:ORMS:main| body text |method:Module.java:131:INFO"
