import FSM.RedundancySms
import FSM.SMS

-- TODO Support sub-states

main = do
    -- putStr $ unlines $ (testChains redundancyFsm (daisyChains $ allTransitions $ tms'specification redundancyFsm))
    putStr $ unlines $ showDaisyChain $ daisyChains $ allTransitions $ tms'specification redundancyFsm
    -- putStr $ unlines $ fsmToDot redundancyFsm
    -- putStr $ unlines $ showHaskell redundancyFsm
