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
data RedAction = Action0
          deriving (Eq, Show)

instance SMaction RedAction

redundancyTransitions = [
      ((Discovery, Timeout),          (Action0, ActiveAlone))
    , ((Discovery, HeartBeatPrime),   (Action0, Active))
    , ((Discovery, HeartBeatSecond),  (Action0, Standby))
    , ((Discovery, HeartBeatCommand), (Action0, Active))
    , ((Active,    HeartBeatOos),     (Action0, Standby))
    , ((Active,    Timeout),          (Action0, ActiveAlone))
    , ((Active,    GoStandby),        (Action0, Standby))
    , ((ActiveAlone, HeartBeatOos),   (Action0, Active))
    , ((ActiveAlone, HeartBeatPrime), (Action0, Active))
    , ((ActiveAlone, HeartBeatSecond),(Action0, Active))
    , ((ActiveAlone, HeartBeatCommand),(Action0, Active))
    , ((Standby,   HeartBeatCommand), (Action0, Active))
    ]

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
                             , tms'actions = [Action0]
                             , tms'initialState = Discovery
                             , tms'terminalStates = []
                             , tms'transitions = redundancyTransitions
                             }

type Code = [String]

testAction :: RedAction -> Code
testAction Action0 = ["checkAction0();"]
-- testAction _ = error "Missing action"

applyEvent :: Tr -> Code
applyEvent e = ["emit" ++ show e ++ "();"]

testState :: FSM -> Code
testState s = ["verifyState( State." ++ show s ++ " );"]

initializeToState :: FSM -> Code
initializeToState Discovery = []
initializeToState s = ["startInState( " ++ show s ++ " );"]

testChain :: [((FSM, Tr), (RedAction, FSM))] -> Code
testChain chain@(start:remainder) =
    (initializeToState (fst $ fst start)) ++ (concat $ map testTransitions chain)
    where
      testTransitions :: ((FSM, Tr), (RedAction, FSM)) -> Code
      testTransitions ((_, ev), (ac, st')) = applyEvent ev ++ testState st' ++ testAction ac

braces :: Code -> Code
braces c = ["{"] ++ indent c ++ ["}"]

indent :: Code -> Code
indent c = map ("    " ++) c

testFn :: String -> [((FSM, Tr), (RedAction, FSM))] -> Code
testFn name steps = [""] ++ indent (["@Test", "public void test" ++ name ++ "() throws Exception"]
                    ++ (braces $ testChain steps))

testChains :: [[((FSM, Tr), (RedAction, FSM))]] -> Code
testChains daisy = concat $ map (\(n,t) -> testFn (show n) t) ([1..] `zip` daisy)

transitionToDot :: ((FSM, Tr), (RedAction, FSM)) -> Code
transitionToDot ((st, ev), (ac, st')) = [show st ++ " -> " ++ show st' ++ 
      "[label=<" ++ show ev ++ ">];"]

fsmToDot :: [((FSM, Tr), (RedAction, FSM))] -> Code
fsmToDot f = ["digraph FSM", "{"] ++ (concat $ map transitionToDot f) ++ ["}"]