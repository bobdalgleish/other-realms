{-# LANGUAGE MultiParamTypeClasses #-}
module Fsm.StateMachine where

import Data.List

class (Eq s, Ord s) => State s where
    initialState   :: s
    terminalStates :: [s]
    allStates      :: [s]

class (Eq e, Ord e) => MachineEvent e where
    allEvents      :: [e]
    
class (Ord s, State s, Ord e, MachineEvent e) => StateMachine s e where
    nextState      :: s -> e -> s
    nextTransition :: s -> e -> Maybe s

reachableStates :: StateMachine s e => [e] -> s -> [s]
reachableStates []     = \st -> []
reachableStates (e:es) = \st -> (nextState st e): (reachableStates es st)

transitiveClosure :: StateMachine s e => [e] -> [s] -> [s]
transitiveClosure events states = closeOver states
                where
                    closeOver sts =
                        let endingStates = nub $ concat $ map (\st -> reachableStates events st) sts
                        in if length endingStates == length sts then sts else closeOver endingStates
