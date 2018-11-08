{-# language GADTs #-}
{-# language AllowAmbiguousTypes #-}
{-# language MultiParamTypeClasses #-}
module FSM.SMS where

import           Data.Maybe (fromMaybe, maybeToList)
import           Data.List (nub, break)
import qualified Data.Map.Strict as Map

class (Ord s, Show s) => SMstate s
class (Ord e, Show e) => SMevent e
class (Eq a, Show a) => SMaction a

data Transition s e a where
    Transition :: (SMstate s, SMevent e, SMaction a) =>
            {
              tx'current :: s
            , tx'event   :: e
            , tx'actions :: [a]
            , tx'next    :: s
            } -> Transition s e a

eqTransition :: (SMstate s, SMevent e, SMaction a) =>
                Transition s e a -> Transition s e a -> Bool
eqTransition tr1 tr2 = (tx'current tr1) == (tx'current tr2) &&
                       (tx'event tr1) == (tx'event tr2) &&
                       (tx'actions tr1) == (tx'actions tr2) &&
                       (tx'next tr1) == (tx'next tr2)
instance (SMstate s, SMevent e, SMaction a) => Eq(Transition s e a) where
    (==) = eqTransition

showTransition :: (SMstate s, SMevent e, SMaction a) => Transition s e a -> String
showTransition (Transition st ev ac st') =
    "((" ++ (show st) ++ ", " ++ (show ev) ++ "), (" ++ (show ac) ++ ", " ++ (show st') ++ "))"

instance (SMstate s, SMevent e, SMaction a) => Show (Transition s e a) where
    show = showTransition

data SmSpec s e a where
    SmSpec :: (SMstate s, SMevent e, SMaction a) =>
               {
                 sms'exit  :: [a]
               , sms'enter :: [a]
               , sms'transitions :: [(e, [a], s)]
               } -> SmSpec s e a

allTransitions :: (SMstate s, SMevent e, SMaction a) =>
                     Map.Map s (SmSpec s e a) -> [Transition s e a]
allTransitions spec = stateSpecToTransitions (Map.keys spec)
    where
        stateSpecToTransitions [] = []
        stateSpecToTransitions (sp':sps) = 
            (map (\(ev, ac, st) -> Transition sp' ev (actionsFromTransition sp' st ac) st) (sms'transitions ((Map.!) spec sp'))) 
                ++ stateSpecToTransitions sps
        actionsFromTransition st st' ac
                              | st == st' = ac
                              | otherwise = (sms'exit ((Map.!) spec st)) ++ ac ++ (sms'enter ((Map.!) spec st'))

allStates :: (SMstate s, SMevent e, SMaction a) =>
             Map.Map s (SmSpec s e a) -> [s]
allStates spec = Map.keys spec

allEvents :: (SMstate s, SMevent e, SMaction a) =>
             Map.Map s (SmSpec s e a) -> [e]
allEvents spec = nub $ map (\(ev, _, _) -> ev) $ concat $ map sms'transitions $ Map.elems spec

allActions :: (SMstate s, SMevent e, SMaction a) =>
              Map.Map s (SmSpec s e a) -> [a]
allActions spec = nub $ concat $ map (\(_, actions, _) -> actions) $ concat $ map sms'transitions $ Map.elems spec


data TMS s e a where
    TMS :: (SMstate s, SMevent e, SMaction a) =>
            { tms'states :: [s]
            , tms'events :: [e]
            , tms'actions :: [a]
            , tms'initialState :: s
            , tms'transitions :: [Transition s e a]
            } -> TMS s e a

nextOperation :: (SMstate s, SMevent e, SMaction a) => TMS s e a
                    -> s -> e -> Maybe ([a],s)
nextOperation sm st ev = fmap (\t -> (tx'actions t, tx'next t)) $ transition (tms'transitions sm) st ev

transition :: (SMstate s, SMevent e, SMaction a) =>
              [Transition s e a] -> s -> e -> Maybe (Transition s e a)
transition [] _ _ = Nothing
transition (t@(Transition st ev _ _):ts) st' ev'
           | st == st' && ev == ev' = Just t
           | otherwise              = transition ts st' ev'


nextTransition :: (SMstate s, SMevent e, SMaction a) => TMS s e a -> s -> e -> Maybe s
nextTransition sm st ev = fmap tx'next $ transition (tms'transitions sm) st ev

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
                [Transition s e a] -> [[Transition s e a]]
daisyChains [] = []
daisyChains (fi:fs) =
    let (chain, universe') = daisyChain1 fi fs
    in chain : daisyChains universe'

daisyChain1 :: (SMstate s, SMevent e, SMaction a) => 
            Transition s e a -> [Transition s e a]
            -> ([Transition s e a], [Transition s e a])
daisyChain1 start candidates =
    let priorEnd = tx'next start
        (nonc, c) = break ((priorEnd==) . tx'current) candidates
    in  if null c
        then ([start], nonc)
        else let (ch, u) = daisyChain1 (head c) (nonc ++ tail c)
             in (start:ch, u)

showDaisyChain :: (SMstate s, SMevent e, SMaction a) => 
                  [[Transition s e a]] -> [String]
showDaisyChain [] = []
showDaisyChain (t: ts) = [""] ++ (map (("    "++) . show) t) ++ showDaisyChain ts