module Main where

import FSM.TSMS
-- import FSM.RedundancySms
import FSM.SMS
import FSM.TableHtml
import FSM.Dot
import FSM.Stateless4j
import FSM.HaskellState
import System.Environment
-- import System.IO

{-
TODO Allow guards on transitions
     A guard is an expression
     All guards for an event return exclusive, perhaps exhaustive, values. This means
     that only one guard can enable a transition. Guards do not have side effects.
TODO Allow ignored events
TODO Allow re-entry on state transition to itself, so that exit/enter
     actions fire
TODO Consider event parameters, also used in guards
TODO Generate stateless4j tests
TODO Dynamic state computation -- No, as guards fully subsume this capability
-}

filepathOf :: String -> FilePath
filepathOf name = "target/" ++ name

writeHtmlTable sm = do
    writeFile (filepathOf $ tms'name sm ++ ".html") (unlines $ 
        (tableStyle ++ fsmToTable sm))

writeDot sm = do
    writeFile (filepathOf $ tms'name sm ++ ".dot") (unlines $ fsmToDot sm)

writeStateless4j sm = do
    writeFile (filepathOf $ tms'name sm ++ "s4j.java") (unlines $ showStateless4j sm)

writeStateless4jTest sm = do
    writeFile (filepathOf $ tms'name sm ++ "s4jTest.java") (unlines $ testStateless4j sm)

writeHaskell sm = do
    writeFile (filepathOf $ tms'name sm ++ "State.hs") (unlines $ showHaskell sm)

selectCommand "haskell" = writeHaskell
selectCommand "stateless4j" = writeStateless4j
selectCommand "stateless4jTest" = writeStateless4jTest
selectCommand "dot" = writeDot
selectCommand "table" = writeHtmlTable

run c = (selectCommand  c) testFsm

main = do
    args <- getArgs
    (selectCommand $ head args) testFsm
    -- putStr $ unlines $ (testChains redundancyFsm (daisyChains $ allTransitions $ tms'specification redundancyFsm))
    -- putStr $ unlines $ showDaisyChain $ daisyChains $ allTransitions $ tms'specification redundancyFsm
    -- putStr $ unlines $ fsmToDot redundancyFsm
    -- putStr $ unlines $ showHaskell redundancyFsm
    -- putStr $ unlines $ showHaskell testFsm
    -- putStr $ unlines $ concat testTransitions
    -- writeDot testFsm
    -- writeHtmlTable testFsm
    -- writeStateless4j testFsm
