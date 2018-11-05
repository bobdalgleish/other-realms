module FSM.RedundancyMachineState where

import FSM.FsmTF


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
          deriving (Eq)

showAction :: RedAction -> String
showAction Action0 = ""

instance Show RedAction where
    show = showAction

instance SMaction RedAction


redundancyTransitions = [
      ((Discovery, Timeout),          (Action0, ActiveAlone))
    , ((Discovery, HeartBeatPrime),   (Action0, Active))
    , ((Discovery, HeartBeatSecond),  (Action0, Standby))
    , ((Discovery, HeartBeatCommand), (Action0, Active))
    , ((Active,    HeartBeatOos),     (Action0, Standby))
    , ((Active,    Timeout),          (Action0, ActiveAlone))
    , ((Active,    GoStandby),        (Action0, Standby))
    , ((Standby,   HeartBeatCommand), (Action0, Active))
    ]


data StateMachine s e a =
    StateMachine { 
                   states :: [s]                    -- ^states that the machine can be in
                 , events :: [e]                    -- ^events that the machine can process
                 , actions :: [a]                   -- ^actions the machine can perform
                 , initialStates' :: [s]             -- ^starting states
                 , transitions :: [((s,e),(a,s))]   -- ^state transitions
                 }

redundancyFsm :: StateMachine FSM Tr RedAction
redundancyFsm = StateMachine {
                               states = [Discovery, Active, ActiveAlone, Standby]
                             , events = [
                                    Timeout
                                , HeartBeatPrime
                                , HeartBeatSecond
                                , HeartBeatOos
                                , HeartBeatCommand
                                , GoStandby
                                ]
                             , actions = [Action0]
                             , initialStates' = [Discovery]
                             , transitions = redundancyTransitions
                             }
