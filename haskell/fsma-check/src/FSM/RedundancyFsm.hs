module FSM.RedundancyFsm where

import FSM.StateMachine


data FSM = 
           Discovery
         | Active
         | ActiveAlone
         | Standby
         deriving (Eq, Ord, Show)

       
data Tr = Timeout
        | HeartBeatPrime
        | HeartBeatSecond
        | HeartBeatOos
        | HeartBeatCommand
        | GoStandby
        deriving (Eq, Ord, Show)

data RedAction = Action0

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
                             , initialStates = [Discovery]
                             , transitions = redundancyTransitions
                             }