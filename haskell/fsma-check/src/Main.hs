-- import FSM.RedundancyFsm
import FSM.RedundancySms
import FSM.SMS
import FSM.TestFsm
-- import FSM.FsmTF
-- import FSM.RedundancyMachineState

main = do
    putStrLn ("Result is " ++ (show $ unreachableStates redundancyFsm))