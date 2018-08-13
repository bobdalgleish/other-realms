import LogRecord.Log
import Data.Time

day = fromGregorian 2018 5 8
tim = picosecondsToDiffTime ((toInteger 18) * 1000000000000)

ts = Timestamp (UTCTime day tim)

sourc = Source "RMS1"
appl = Application "ORMS"
threa = Thread "main"
bod = Body "Starting up with parameters []"
mname = MethodName "main"
mlineno = MethodLineNumber "18"
modul = ModuleName "RmsMain.java"
ll = LogInfo

lr = Log { timestamp = ts, source = sourc, application = appl, thread = threa, body = bod, methodName = mname,
           moduleName = modul, logLevel = ll }
     
