module Browser.Main where

import System.IO
import System.Environment
import LogRecord.MatchRmsLog
import LogRecord.Log
import Browser.Context
import Browser.Rms
import Browser.Predicate

readFileFromContext :: FilePath -> IO [Log String]
readFileFromContext f = do
    contents <- readFile f
    return $ matchRms Nothing (lines contents)

makeFileContext :: String -> LogContext
makeFileContext f = makeContext (f :: FilePath)

getFile :: LogContext -> IO LogContext
getFile ctx = do
    logs <- readFileFromContext $ file ctx
    return (ctx {logLines = logs})

selectFilter :: String -> Predicate (Log String)
selectFilter "signpost" = rmsSignposts
selectFilter "configfile" = testPredicate configFileHolder
selectFilter _          = Success

selectPrint :: String -> PrettyPrinter
selectPrint "short" = PrettyPrinter showShortFields
selectPrint "long" = PrettyPrinter show
selectPrint _ = PrettyPrinter showFields

getFilter :: LogContext -> String -> LogContext
getFilter ctx filt = ctx { filters = selectFilter filt }

applyContext :: LogContext -> IO ()
applyContext ctx = do
    mapM_ (putStrLn . (printLog ctx)) $ filter (applyPredicate (filters ctx)) (logLines ctx) 

getFileArg :: [String] -> IO String
getFileArg (f:_) = return f
getFileArg _ = do
    putStr "Enter file path: "
    getLine

getFilterArg :: [String] -> IO String
getFilterArg (_:f:_) = return f
getFilterArg _ = do
    putStr "Enter filter name: "
    getLine

splitCommand :: String -> (String, String)
splitCommand s = let pieces = words s
                 in case pieces of
                    (cmd:[])      -> (cmd, "")
                    (cmd:args)    -> (cmd, unwords args)
                    _             -> ("?", "?")

commandLine :: LogContext -> IO ()
commandLine ctx = do
                    putStr "Enter command: "
                    line <- getLine
                    let (command, arg) = splitCommand line
                    case command of
                        "file" -> do
                            fileCtx <- getFile $ ctx {file = arg}
                            commandLine fileCtx
                        "filter" -> do
                            commandLine $ getFilter ctx arg
                        "print" -> do
                            commandLine $ ctx {prettyPrinter = selectPrint arg}
                        "run" -> do
                            applyContext ctx
                            commandLine ctx
                        "quit" -> do
                            putStrLn "Goodby"
                        _ -> do
                            putStrLn "?"
                            commandLine ctx

main :: IO ()
main = do
    args <- getArgs
    path <- getFileArg args
    fname <- getFilterArg args
    ctx <- getFile $ makeFileContext path
    commandLine $ getFilter ctx fname
