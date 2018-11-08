-- import FSM.RedundancyFsm
import FSM.RedundancySms
import FSM.SMS
-- import FSM.FsmTF
-- import FSM.RedundancyMachineState

redTests = daisyChains $ tms'transitions redundancyFsm

main = do
    putStr $ unlines $ testChains redTests
    -- putStr $ unlines $ fsmToDot redundancyTransitions'