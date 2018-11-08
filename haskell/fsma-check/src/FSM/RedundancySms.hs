module FSM.RedundancySms where

import FSM.SMS

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

-- |Actions (placeholder)
data RedAction = 
          StartHeartBeatTimer
        | CancelHeartBeatTimer
        | SendHeartBeatNow
        | CancelDiscoveryTimer
        deriving (Eq, Show)

instance SMaction RedAction

redundancySpec :: [SmSpec FSM Tr RedAction]
redundancySpec = [
      SmSpec Discovery [] [] [
        (Timeout, [], ActiveAlone)
      , (HeartBeatPrime, [], Active)
      , (HeartBeatSecond, [], Standby)
      , (HeartBeatCommand, [], Active)
      ]
    , SmSpec Active [] [] [
        (HeartBeatOos, [], Standby)
      , (Timeout, [], ActiveAlone)
      , (GoStandby, [], Standby)
      ]
    , SmSpec ActiveAlone [] [] [
        (HeartBeatOos, [], Active)
      , (HeartBeatPrime, [], Active)
      , (HeartBeatSecond, [], Active)
      , (HeartBeatCommand, [], Active)
      ]
    , SmSpec Standby [] [] [
        (HeartBeatCommand, [], Active)
      ]
    ]

redundancyTransitions' :: [Transition FSM Tr RedAction]
redundancyTransitions' = specToTransitions redundancySpec

redundancyFsm :: TMS FSM Tr RedAction
redundancyFsm = TMS {
                               tms'states = [Discovery, Active, ActiveAlone, Standby]
                             , tms'events = [
                                    Timeout
                                , HeartBeatPrime
                                , HeartBeatSecond
                                , HeartBeatOos
                                , HeartBeatCommand
                                , GoStandby
                                ]
                             , tms'actions = []
                             , tms'initialState = Discovery
                             , tms'terminalStates = []
                             , tms'transitions = redundancyTransitions'
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