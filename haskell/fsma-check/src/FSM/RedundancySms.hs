module FSM.RedundancySms where

import FSM.SMS
import qualified Data.Map.Strict as Map

-- |Redundancy finite state machine states
data FSM = 
              Discovery
            | Active
            | ActiveAlone
            | Standby
            deriving (Eq, Ord, Show)

instance SMstate FSM where
  showDotState st = show st

-- |Events that can occur
data Tr = 
              Timeout
            | HeartBeatPrime
            | HeartBeatSecond
            | HeartBeatOos
            | HeartBeatCommand
            | GoStandby
            deriving (Eq, Ord, Show)

instance SMevent Tr where
  showDotEvent HeartBeatPrime   = "HB<sub>Prime</sub>"
  showDotEvent HeartBeatSecond  = "HB<sub>Second</sub>"
  showDotEvent HeartBeatOos     = "HB<sub>OOS</sub>"
  showDotEvent HeartBeatCommand = "HB<sub>Command</sub>"
  showDotEvent ev               = show ev

-- |Actions
data RedAction = 
          StartHeartBeatTimer
        | CancelHeartBeatTimer
        | SendHeartBeatNow
        | CancelDiscoveryTimer
        deriving (Eq, Show)

instance SMaction RedAction where
  showDotAction ac = show ac

redundancySpec :: Map.Map FSM (SmSpec FSM Tr RedAction)
redundancySpec = Map.fromList [
      (Discovery, SmSpec [CancelDiscoveryTimer, StartHeartBeatTimer] [] [
        (Timeout, [], ActiveAlone)
      , (HeartBeatPrime, [], Active)
      , (HeartBeatSecond, [], Standby)
      , (HeartBeatCommand, [], Active)
      ])
    , (Active, SmSpec [] [SendHeartBeatNow] [
        (HeartBeatOos, [], Standby)
      , (Timeout, [], ActiveAlone)
      , (GoStandby, [], Standby)
      ])
    , (ActiveAlone, SmSpec [] [SendHeartBeatNow] [
        (HeartBeatOos, [], Active)
      , (HeartBeatPrime, [], Active)
      , (HeartBeatSecond, [], Active)
      , (HeartBeatCommand, [SendHeartBeatNow], Active)
      ])
    , (Standby, SmSpec [] [SendHeartBeatNow] [
        (HeartBeatCommand, [SendHeartBeatNow], Active)
      ])
    ]

redundancyFsm :: TMS FSM Tr RedAction
redundancyFsm = mkTms "RedundancyFsm" redundancySpec Discovery

type Code = [String]

testAction :: RedAction -> Code
testAction ac = ["check" ++ (show ac) ++ "();"]
-- testAction _ = error "Missing action"

applyEvent :: Tr -> Code
applyEvent e = ["emit" ++ show e ++ "();"]

testState :: FSM -> Code
testState s = ["verifyState( State." ++ show s ++ " );"]

initializeToState :: FSM -> Code
initializeToState Discovery = []
initializeToState s = ["startInState( " ++ show s ++ " );"]

testChain :: [Transition FSM Tr RedAction] -> Code
testChain chain@(start:remainder) =
    (initializeToState (tx'current start)) ++ (concat $ map testTransitions chain)
    where
      testTransitions :: Transition FSM Tr RedAction -> Code
      testTransitions (Transition _ ev ac st') =
         applyEvent ev ++ testState st' ++ (concat $ map testAction ac)

braces :: Code -> Code
braces c = ["{"] ++ indent c ++ ["}"]

indent :: Code -> Code
indent c = map ("    " ++) c

testFn :: String -> [Transition FSM Tr RedAction] -> Code
testFn name steps = [""] ++ ["@Test", "public void test" ++ name ++ "() throws Exception"]
                    ++ (braces $ testChain steps)

testChains :: TMS FSM Tr RedAction -> [[Transition FSM Tr RedAction]] -> Code
testChains sm daisy = ["", "class " ++ (tms'name sm) ++ "Test"] ++ (braces $ 
    concat $ map (\(n,t) -> testFn (show n) t) ([1..] `zip` daisy))

transitionToDot :: Transition FSM Tr RedAction -> Code
transitionToDot tx = [showDotState (tx'current tx) ++ " -> " ++ showDotState (tx'next tx) ++ 
      "[label=<" ++ showDotEvent (tx'event tx) ++ ">];"]

fsmToDot :: TMS FSM Tr RedAction -> [Transition FSM Tr RedAction] -> Code
fsmToDot sm f = ["digraph " ++ (tms'name sm) ++ " {"] ++ (concat $ map transitionToDot f) ++ ["}"]