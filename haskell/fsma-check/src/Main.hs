import FSM.TSMS
-- import FSM.RedundancySms
import FSM.SMS
-- import System.IO

-- TODO output table of transitions - HTML or XML
-- TODO show nested states in table

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
    writeHtmlTable testFsm
