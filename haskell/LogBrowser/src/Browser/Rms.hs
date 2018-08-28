module Browser.Rms where

import LogRecord.Log
import Browser.Context
import Browser.Predicate

rmsSignposts :: Predicate (Log String)
rmsSignposts = AnyOf $ map (\s -> Test $ bodyHas s)
                           ["RMS redundancy state change to RS_",
                            "Starting up with arguments:",
                            "Updating operational state from OS",
                            "Startup delay value is "]

configFileHolder :: Log String -> Bool
configFileHolder = moduleIs "ConfigFileHolder.java"