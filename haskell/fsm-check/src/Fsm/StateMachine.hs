{-# LANGUAGE MultiParamTypeClasses #-}
module Fsm.StateMachine where

import qualified Data.Set as Set

class (Eq s, Ord s) => State s where
    initialState   :: s
    terminalStates :: [s]
    allStates      :: [s]

class (Eq e, Ord e) => MachineEvent e where
    allEvents      :: [e]
    
class (State s, MachineEvent e) => StateMachine s e where
    nextState      :: s -> e -> s


reachableStates :: StateMachine s e => [e] -> s -> Set.Set s
reachableStates []     = \st -> Set.empty
reachableStates (e:es) = \st -> Set.insert (nextState st e) (reachableStates es st)

transitiveClosure :: StateMachine s e => [e] -> Set.Set s -> Set.Set s
transitiveClosure events states = closeOver states
                where
                    closeOver :: Set.Set s -> Set.Set s
                    closeOver sts =
                        let nds = Set.map (reachableStates events) sts
                            endingStates = foldl Set.union sts nds
                        in if length endingStates == length sts then sts else closeOver endingStates