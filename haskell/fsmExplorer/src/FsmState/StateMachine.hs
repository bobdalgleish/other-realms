module FsmState.StateMachine where

import Data.Maybe

data McState = Discovery
             | Standby
             | Active
             | ActiveOnly
             deriving (Eq, Show)

startState = Discovery
stateList = [Discovery, Standby, Active, ActiveOnly]

data McOp = Timeout
          | HeartBeatPrime
          | HeartBeatSecond
          | GoStandby
          deriving (Eq, Show)

opList = [Timeout, HeartBeatPrime, HeartBeatSecond, GoStandby]

type Transition = (McState, McOp, McState)
type TransitionOp = ((McState, McOp), McState)

transitions :: [Transition]
transitions = [
                (Discovery, Timeout, Active)
              , (Discovery, HeartBeatPrime, Active)
              , (Discovery, HeartBeatSecond, Standby)
              , (Standby, Timeout, Active)
              , (Standby, HeartBeatPrime, Active)
              , (Standby, HeartBeatSecond, Standby)
              , (Active, Timeout, Active)
              , (Active, HeartBeatPrime, Active)
              , (Active, HeartBeatSecond, Active)
    ]

transitionTable :: [TransitionOp]
transitionTable = map remap transitions
                  where
                    remap :: Transition -> TransitionOp
                    remap (st, op, nst) = ((st, op), nst)

go :: McState -> McOp -> Maybe McState
go st op = lookup (st, op) transitionTable

goAll :: McState -> [McState]
goAll st = catMaybes $ map (\o -> go st o) opList

reachable :: McState -> McState -> Bool
reachable st nd = any (nd ==) (goAll st)


allReachable :: Bool
allReachable = all anyReachable stateList
                    
anyReachable :: McState -> Bool
anyReachable nd = any (flip reachable nd) stateList

-- |List of states that cannot be reached
unreachable :: [McState]
unreachable = [nd | nd <- stateList, (not (anyReachable nd)) && (nd /= startState)]
