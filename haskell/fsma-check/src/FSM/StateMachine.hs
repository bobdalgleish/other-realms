module FSM.StateMachine where

import Data.List

class (Ord s, Show s) => FsmState s

class (Ord e, Show e) => FsmEvent e

class (Eq a, Show a) => FsmAction a where
    isEmpty :: a -> Bool
    isError :: a -> Bool

data StateMachine s e a =
    StateMachine { 
                   states :: [s]                    -- ^states that the machine can be in
                 , events :: [e]                    -- ^events that the machine can process
                 , actions :: [a]                   -- ^actions the machine can perform
                 , initialStates :: [s]             -- ^starting states
                 , transitions :: [((s,e),(a,s))]   -- ^state transitions
                 }

{-         
operate :: (Eq s, Eq e) => (s, StateMachine s e a) -> e -> Maybe (a,s)
operate sm ev 
 -}
-- |Find the next state from this state for an event, or stay in this state
nextState :: (Ord s, Ord e, Eq a) => StateMachine s e a -> s -> e -> s
nextState sm st ev = case nextTransition sm st ev of
                        Just st' -> st'
                        Nothing  -> st

-- |Find the next state from this state for an event, if present
nextTransition :: (Ord s, Ord e, Eq a) => StateMachine s e a -> s -> e -> Maybe s
nextTransition sm st ev = fmap snd $ nextOperation sm st ev

-- |Find the action and next state for an event in the given state
nextOperation :: (Ord s, Ord e, Eq a) => StateMachine s e a -> s -> e -> Maybe (a, s)
nextOperation sm st ev = lookup (st, ev) (transitions sm)



allNextStates :: (Ord s, Ord e, Eq a) => StateMachine s e a -> s -> [e] -> [s]
allNextStates _ _ [] = []
allNextStates sm st (ev:es) = case nextTransition sm st ev of
                                Just st' -> st': allNextStates sm st es
                                Nothing  -> allNextStates sm st es

-- |Compute the set of states reachable from a state
reachableStates :: (Ord s, Ord e, Eq a) => StateMachine s e a -> s -> [s]
reachableStates sm st = nub $ allNextStates sm st (events sm)

-- |Compute the transitive closure from the initial states
transitiveClosure :: (Ord s, Ord e, Eq a) => StateMachine s e a -> [s]
transitiveClosure sm = transitives sm (initialStates sm) (initialStates sm)
    where
        transitives :: (Ord s, Ord e, Eq a) => StateMachine s e a -> [s] -> [s] -> [s]
        transitives _ [] reach = reach
        transitives stm reachable@(st: sts) reach =
             let r = reachableStates stm st
                 rs = [r' | r' <- r, not (r' `elem` reach)]
             in
                if null rs
                then transitives stm sts reach
                else transitives stm (sts ++ rs) (reach ++ rs)

-- |Compute the set of unreachable states
unreachableStates :: (Ord s, Ord e, Eq a) => StateMachine s e a -> [s]
unreachableStates sm = 
    let rs = transitiveClosure sm
    in [r' | r' <- states sm, not (r' `elem` rs)]