{-# language GADTs #-}
{-# language AllowAmbiguousTypes #-}
{-# language MultiParamTypeClasses #-}
module FSM.SMS where

-- import Control.Monad.State
import Data.Maybe (fromMaybe, maybeToList)
import Data.List (nub)

class (Ord s, Show s) => SMstate s
class (Ord e, Show e) => SMevent e
class (Eq a, Show a) => SMaction a

data TMS s e a where
    TMS :: (SMstate s, SMevent e, SMaction a) =>
            { tms'states :: [s]
            , tms'events :: [e]
            , tms'actions :: [a]
            , tms'initialState :: s
            , tms'terminalStates :: [s]
            , tms'transitions :: [((s,e),(a,s))]
            } -> TMS s e a

nextOperation :: (SMstate s, SMevent e, SMaction a) => TMS s e a
                    -> s -> e -> Maybe (a,s)
nextOperation sm st ev = lookup (st, ev) (tms'transitions sm)

nextTransition :: (SMstate s, SMevent e, SMaction a) => TMS s e a -> s -> e -> Maybe s
nextTransition sm st ev = fmap snd $ nextOperation sm st ev

nextState :: (SMstate s, SMevent e, SMaction a) => TMS s e a -> s -> e -> s
nextState sm st ev = fromMaybe st $ nextTransition sm st ev

transitiveClosure :: (SMstate s, SMevent e, SMaction a) => TMS s e a -> [s]
transitiveClosure sm = allTransitions sm [tms'initialState sm] [tms'initialState sm]
    where
        allTransitions :: (SMstate s, SMevent e, SMaction a) => TMS s e a -> [s] -> [s] -> [s]
        allTransitions sm [] reach = reach
        allTransitions sm reachable@(st:sts) reach =
            let r = reachables sm st
                rs = [r' | r' <- r, r' `notElem` reach]
            in
                if null rs
                then allTransitions sm sts reach
                else allTransitions sm (sts ++ rs) (reach ++ rs)
        reachables :: (SMstate s, SMevent e, SMaction a) => TMS s e a -> s -> [s]
        reachables sm st = nub $ allNexts sm st (tms'events sm)
        allNexts :: (SMstate s, SMevent e, SMaction a) => TMS s e a -> s -> [e] -> [s]
        allNexts sm _ [] = []
        allNexts sm st (ev:evs) = (maybeToList $ nextTransition sm st ev) ++ allNexts sm st evs

unreachableStates :: (SMstate s, SMevent e, SMaction a) => TMS s e a -> [s]
unreachableStates sm =
    let rs = transitiveClosure sm
    in [r' | r' <- tms'states sm, r' `notElem` rs]