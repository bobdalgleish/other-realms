module Browser.Rms where

import LogRecord.Log
import Browser.Context
import Browser.Predicate
import Data.List (isInfixOf)

rmsSignposts :: Predicate (Log String)
rmsSignposts = AnyOf $ map (\s -> Test $ bodySearch s)
                           ["RMS redundancy state change to RS_",
                            "Starting up with arguments:",
                            "Updating operational state from OS",
                            "Startup delay value is "]


bodySearch :: String -> Log String -> Bool
bodySearch p = \l -> let (Body b) = body l
                     in p `isInfixOf` b

configFileHolder :: Log String -> Bool
configFileHolder log = let (ModuleName m) = moduleName log
                       in m == "ConfigFileHolder.java"