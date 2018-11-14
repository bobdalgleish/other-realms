import FSM.RedundancySms
import FSM.SMS


main = do
    -- putStr $ unlines $ (testChains redundancyFsm (daisyChains $ tms'transitions redundancyFsm))
    putStr $ unlines $ fsmToDot redundancyFsm