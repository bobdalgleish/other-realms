import FSM.TSMS
-- import FSM.RedundancySms
import FSM.SMS
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
TODO Generate stateless4j code
TODO Generate stateless4j tests
TODO Dynamic state computation -- No, as guards fully subsume this capability
-}

writeHtmlTable sm = do
    writeFile (tms'name sm ++ ".html") (unlines $ fsmToTable sm)

main = do
    -- putStr $ unlines $ (testChains redundancyFsm (daisyChains $ allTransitions $ tms'specification redundancyFsm))
    -- putStr $ unlines $ showDaisyChain $ daisyChains $ allTransitions $ tms'specification redundancyFsm
    -- putStr $ unlines $ fsmToDot redundancyFsm
    -- putStr $ unlines $ showHaskell redundancyFsm
    -- putStr $ unlines $ showHaskell testFsm
    -- putStr $ unlines $ concat testTransitions
    -- putStr $ unlines $ fsmToDot testFsm
    -- writeHtmlTable testFsm
    putStr $ unlines $ showStateless4j testFsm
