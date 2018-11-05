{-# language TypeFamilies #-}
{-# language MultiParamTypeClasses #-}
module FSM.FsmTF where

import Data.Maybe (maybeToList)
import Data.List  (nub)

class (Ord s, Show s) => SMstate s
class (Ord e, Show e) => SMevent e
class (Eq a, Show a) => SMaction a
    
class (SMstate s, SMevent e, SMaction a) => MachineState s e a where
    data MS s e a
    allEvents      :: MS s e a -> [e]
    initialStates  :: MS s e a -> [s]
    allStates      :: MS s e a -> [s]
    allActions     :: MS s e a -> [a]
    nextOperation  :: MS s e a -> s -> e -> Maybe (a,s)
    nextTransition :: MS s e a -> s -> e -> Maybe s
    nextTransition sm st ev = fmap snd $ nextOperation sm st ev
    nextState      :: MS s e a -> s -> e -> s
    nextState sm st ev = case nextTransition sm st ev of
                            Just st' -> st'
                            Nothing  -> st
    allNextStates  :: MS s e a -> s -> [e] -> [s]
    allNextStates _ _ [] = []
    allNextStates sm st (ev:evs) =
        let nextStates = allNextStates sm st evs
        in  (maybeToList $ nextTransition sm st ev) ++ nextStates
    reachableStates :: MS s e a -> s -> [s]
    reachableStates sm s = nub $ allNextStates sm s (allEvents sm)
    transitiveClosure :: MS s e a -> [s]
    transitiveClosure sm = transitivesOf sm (initialStates sm) (initialStates sm)
        where
            transitivesOf :: MachineState s e a => MS s e a -> [s] -> [s] -> [s]
            transitivesOf _ [] reach = reach
            transitivesOf sm reachable@(st:sts) reach =
                let r = reachableStates sm st
                    rs = [r' | r' <- r, r' `notElem` reach]
                in
                    if null rs
                    then transitivesOf sm sts reach
                    else transitivesOf sm (sts ++ rs) (reach ++ rs)