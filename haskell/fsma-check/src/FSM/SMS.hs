{-# language GADTs #-}
{-# language AllowAmbiguousTypes #-}
{-# language MultiParamTypeClasses #-}
module FSM.SMS where

-- import Control.Monad.State
import Data.Maybe (fromMaybe, maybeToList)
import Data.List (nub, break)

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

-- |Return all sequences where start state of subsequent matches end state of previous
daisyChains :: (SMstate s, SMevent e, SMaction a) => 
                [((s, e), (a, s))] -> [[((s, e), (a, s))]]
daisyChains [] = []
daisyChains (fi:fs) =
    let (chain, universe') = daisyChain1 fi fs
    in chain : daisyChains universe'

daisyChain1 :: (SMstate s, SMevent e, SMaction a) => 
            ((s, e), (a, s)) -> [((s, e), (a, s))]
            -> ([((s, e), (a, s))], [((s, e), (a, s))])
daisyChain1 start candidates =
    let priorEnd = snd $ snd start
        (nonc, c) = break ((priorEnd==) . fst . fst) candidates
    in  if null c
        then ([start], nonc)
        else let (ch, u) = daisyChain1 (head c) (nonc ++ tail c)
             in (start:ch, u)

showDaisyChain :: (SMstate s, SMevent e, SMaction a) => 
                  [[((s, e), (a, s))]] -> [String]
showDaisyChain [] = []
showDaisyChain (t: ts) = [""] ++ (map (("    "++) . show) t) ++ showDaisyChain ts