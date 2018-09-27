{-# LANGUAGE MultiParamTypeClasses #-}
module Fsm.Fsm1 where

import Fsm.StateMachine
import qualified Data.Set as Set

data FSM = Start
         | Discovery
         | Active
         | Standby
         deriving (Eq, Ord, Show)

instance State FSM where
    initialState = Start
    terminalStates = []
    allStates = [Start, Discovery, Active, Standby]
        
data Tr = StartUp
        | Timeout
        | HeartBeatPrime
        | HeartBeatSecond
        | HeartBeatOos
        | HeartBeatCommand
        deriving (Eq, Ord, Show)

instance MachineEvent Tr where
    allEvents = [
                  StartUp
                , Timeout
                , HeartBeatPrime
                , HeartBeatSecond
                , HeartBeatOos
                , HeartBeatCommand
                ]

instance StateMachine FSM Tr where
    nextState f e = case lookup (f, e) transitions of
                     Just nd -> nd
                     Nothing -> f

transitions :: [((FSM, Tr), FSM)]
transitions = [
                ((Start, StartUp), Discovery)
                , ((Discovery, Timeout), Active)
                , ((Discovery, HeartBeatPrime), Active)
                , ((Discovery, HeartBeatSecond), Standby)
                , ((Discovery, HeartBeatCommand), Active)
                , ((Active, HeartBeatOos), Standby)
                , ((Standby, HeartBeatCommand), Active)
                ]

closure :: Set.Set FSM
closure = closeOver $ Set.singleton (initialState :: FSM)
          where
            closeOver :: Set.Set FSM -> Set.Set FSM
            closeOver sts =
              let nds = Set.map (reachableStates (allEvents :: [Tr])) sts
                  endingStates = foldl Set.union sts nds
              in if endingStates == sts then endingStates else closeOver endingStates

closureFsm :: Set.Set FSM
closureFsm = transitiveClosure (allEvents :: [Tr]) (Set.singleton (initialState :: FSM))