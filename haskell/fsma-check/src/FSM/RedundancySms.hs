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
      (Discovery, SmSpec [CancelDiscoveryTimer, StartHeartBeatTimer] [] []
       (Map.fromList [
        (Timeout, ([], ActiveAlone))
      , (HeartBeatPrime, ([], Active))
      , (HeartBeatSecond, ([], Standby))
      , (HeartBeatCommand, ([], Active))
      ]))
    , (Active, SmSpec [] [SendHeartBeatNow] [ActiveAlone]
       (Map.fromList [
        (HeartBeatOos, ([], Standby))
      , (Timeout, ([], ActiveAlone))
      , (GoStandby, ([], Standby))
      ]))
    , (ActiveAlone, SmSpec [] [] []
       (Map.fromList [
        (HeartBeatOos, ([], Active))
      , (HeartBeatPrime, ([], Active))
      , (HeartBeatSecond, ([], Active))
      , (HeartBeatCommand, ([SendHeartBeatNow], Active))
      ]))
    , (Standby, SmSpec [] [SendHeartBeatNow] []
       (Map.fromList [
        (HeartBeatCommand, ([SendHeartBeatNow], Active))
        , (Timeout, ([], ActiveAlone))
      ]))
    ]

redundancyFsm :: TMS FSM Tr RedAction
redundancyFsm = mkTms "RedundancyFsm" redundancySpec Discovery

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

testFn :: String -> [Transition FSM Tr RedAction] -> Code
testFn name steps = ["", "@Test"] ++ 
              braceGroup ("public void test" ++ name)
                    (testChain steps)

testChains :: TMS FSM Tr RedAction -> [[Transition FSM Tr RedAction]] -> Code
testChains sm daisy = [""] ++ 
    braceGroup ("class " ++ (tms'name sm) ++ "Test")
      (concat $ map (\(n,t) -> testFn (show n) t) ([1..] `zip` daisy))
