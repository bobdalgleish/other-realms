{-# LANGUAGE MultiParamTypeClasses #-}
module Fsm.StateMachine where

import Data.List

class Ord s => State s where
    initialState   :: s
    terminalStates :: [s]
    allStates      :: [s]

class Ord e => MachineEvent e where
    allEvents      :: [e]
    
class Eq a => Action a

class (Ord s, State s, Ord e, MachineEvent e) => StateMachine s e where
    nextState      :: s -> e -> s
    nextTransition :: s -> e -> Maybe s

-- |Compute the set of states reachable from an initial state
reachableStates :: StateMachine s e => [e] -> s -> [s]
reachableStates []     = \_ -> []
reachableStates (e:es) = \st -> (nextState st e): (reachableStates es st)

-- |Compute the transitive closure from the set of initial states
transitiveClosure :: StateMachine s e => [e] -> [s] -> [s]
transitiveClosure events states = closeOver states
                where
                    closeOver sts =
                        let endingStates = nub $ concat $ map (\st -> reachableStates events st) sts
                        in if length endingStates == length sts then sts else closeOver endingStates
