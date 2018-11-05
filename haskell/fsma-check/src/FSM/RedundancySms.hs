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
                             , tms'terminalStates = [Active, Standby]
                             , tms'transitions = redundancyTransitions
                             }
