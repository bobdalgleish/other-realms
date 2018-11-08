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

instance SMstate FSM

-- |Events that can occur
data Tr = 
              Timeout
            | HeartBeatPrime
            | HeartBeatSecond
            | HeartBeatOos
            | HeartBeatCommand
            | GoStandby
            deriving (Eq, Ord, Show)

instance SMevent Tr

-- |Actions
data RedAction = 
          StartHeartBeatTimer
        | CancelHeartBeatTimer
        | SendHeartBeatNow
        | CancelDiscoveryTimer
        deriving (Eq, Show)

instance SMaction RedAction

redundancySpec :: Map.Map FSM (SmSpec FSM Tr RedAction)
redundancySpec = Map.fromList [
      (Discovery, SmSpec [CancelDiscoveryTimer, StartHeartBeatTimer] [] [
        (Timeout, [], ActiveAlone)
      , (HeartBeatPrime, [], Active)
      , (HeartBeatSecond, [], Standby)
      , (HeartBeatCommand, [], Active)
      ])
    , (Active, SmSpec [] [] [
        (HeartBeatOos, [], Standby)
      , (Timeout, [], ActiveAlone)
      , (GoStandby, [], Standby)
      ])
    , (ActiveAlone, SmSpec [] [] [
        (HeartBeatOos, [], Active)
      , (HeartBeatPrime, [], Active)
      , (HeartBeatSecond, [], Active)
      , (HeartBeatCommand, [], Active)
      ])
    , (Standby, SmSpec [] [] [
        (HeartBeatCommand, [SendHeartBeatNow], Active)
      ])
    ]

redundancyFsm :: TMS FSM Tr RedAction
redundancyFsm = TMS {
                               tms'states = allStates redundancySpec
                             , tms'events = allEvents redundancySpec
                             , tms'actions = allActions redundancySpec
                             , tms'initialState = Discovery
                             , tms'transitions = allTransitions redundancySpec
                             }

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
testFn name steps = [""] ++ indent (["@Test", "public void test" ++ name ++ "() throws Exception"]
                    ++ (braces $ testChain steps))

testChains :: [[Transition FSM Tr RedAction]] -> Code
testChains daisy = concat $ map (\(n,t) -> testFn (show n) t) ([1..] `zip` daisy)

transitionToDot :: Transition FSM Tr RedAction -> Code
transitionToDot tx = [show (tx'current tx) ++ " -> " ++ show (tx'next tx) ++ 
      "[label=<" ++ show (tx'event tx) ++ ">];"]

fsmToDot :: [Transition FSM Tr RedAction] -> Code
fsmToDot f = ["digraph FSM", "{"] ++ (concat $ map transitionToDot f) ++ ["}"]