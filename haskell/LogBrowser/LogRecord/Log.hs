module LogRecord.Log where
import LogRecord.Timestamp

-- |Source of the log, such as server
data Source = Source String deriving Show

-- |Application that generated the log
data Application a =  Application a deriving Show

-- |Body text of the log
data Body a = Body a deriving Show

-- |Thread identifier
data Thread a = Thread a deriving Show

-- |Method identifier
data MethodName a = MethodName a deriving Show

-- |Module line number
data MethodLineNumber a = MethodLineNumber a deriving Show

-- |Name of the module
data ModuleName a = ModuleName a deriving Show

data LogLevel = LogError | LogWarn | LogInfo | LogDebug

showLogLevel :: LogLevel -> String
showLogLevel LogError = "Error"
showLogLevel LogWarn  = "Warn"
showLogLevel LogInfo  = "Info"
showLogLevel LogDebug = "Debug"

instance Show LogLevel where show = showLogLevel

toLogLevel :: String -> LogLevel
toLogLevel "ERROR" = LogError
toLogLevel "WARN"  = LogWarn
toLogLevel "INFO"  = LogInfo
toLogLevel "DEBUG" = LogDebug

data Log a = Log { timestamp :: Timestamp
                 , source :: Source
                 , application :: Application a
                 , thread :: Thread a
                 , body :: Body a
                 , methodName :: MethodName a
                 , moduleName :: ModuleName a
                 , moduleLineNo :: MethodLineNumber a
                 , logLevel :: LogLevel
                 }
           deriving Show

newLog :: Timestamp -> Source -> Application a -> Thread a -> a -> Log a
newLog t src app thr s = Log { timestamp = t,
                   source = src,
                   application = app,
                   thread = thr,
                   body = Body s,
                   methodName = MethodName s,
                   moduleName = ModuleName s,
                   moduleLineNo = MethodLineNumber s,
                   logLevel = LogInfo
                     }
