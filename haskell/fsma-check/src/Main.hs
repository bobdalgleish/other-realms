import FSM.RedundancySms
import FSM.SMS

-- TODO Support sub-states
-- TODO Pull daisyChain1 into daisyChains
-- TODO Emit Haskell FSM

main = do
    -- putStr $ unlines $ (testChains redundancyFsm (daisyChains $ tms'transitions redundancyFsm))
    -- putStr $ unlines $ fsmToDot redundancyFsm
    putStr $ unlines $ showHaskell redundancyFsm
