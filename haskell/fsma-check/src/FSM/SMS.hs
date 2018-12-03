{-# language GADTs #-}
{-# language AllowAmbiguousTypes #-}
{-# language MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
module FSM.SMS where

import           Data.Maybe (fromMaybe, fromJust, maybeToList)
import           Data.List (break, (\\))
import           FSM.Code
import qualified Data.Map.Strict as Map
-- import Debug.Trace

class (Ord s, Show s) => SMstate s where
    showDotState :: s -> String
class (Ord e, Show e) => SMevent e where
    showDotEvent :: e -> String
class (Eq a, Show a) => SMaction a where
    showDotAction :: a -> String

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

showDotTransition :: (SMstate s, SMevent e) => s -> e -> s -> Code
showDotTransition st ev  st' = [showDotState st ++ " -> " ++ showDotState st' ++ 
    "[label=<" ++ showDotEvent ev ++ ">];"]

instance (SMstate s, SMevent e, SMaction a) => Show (Transition s e a) where
    show = showTransition

data SmSpec s e a where
    SmSpec :: (SMstate s, SMevent e, SMaction a) =>
               {
                 sms'exit  :: [a]                       -- ^Actions to perform on exit
               , sms'enter :: [a]                       -- ^Actions to perform on entry
               , sms'substates :: [s]                   -- ^Substates of this state
               , sms'transitions :: Map.Map e ([a], s)  -- ^State transitions
               } -> SmSpec s e a

mkTransition :: (SMstate s, SMevent e, SMaction a) => [(e, ([a], s))] -> SmSpec s e a
mkTransition m = SmSpec [] [] [] (Map.fromList m)

data TMS s e a where
    TMS :: (SMstate s, SMevent e, SMaction a) =>
            { tms'name          :: String                   -- ^Name of the state machine
            , tms'initialState  :: s                        -- ^Initial state
            , tms'childParent   :: Map.Map s s              -- ^Map from child state to parent
            , tms'specification :: Map.Map s (SmSpec s e a) -- ^Transition specification
            } -> TMS s e a

mkTms :: (SMstate s, SMevent e, SMaction a) =>
         String -> Map.Map s (SmSpec s e a) -> s -> TMS s e a
mkTms name spec initial =
    let childrenOf = map (\(st, SmSpec _ _ subs _) -> (st, subs)) $ Map.assocs spec
        childPlusParent = map (\(p, c) -> map (\stc -> (stc, p)) c) childrenOf
        parentMap = Map.fromList $ concat childPlusParent
    in TMS {
              tms'name          = name
            , tms'initialState  = initial
            , tms'childParent   = parentMap
            , tms'specification = spec
            }

parentOf :: (SMstate s, SMevent e, SMaction a) =>
            TMS s e a -> s -> s
parentOf sm state = fromMaybe state $ ((Map.!?) (tms'childParent sm) state)

-- |Order states (lexically) with substates following parents
orderStates :: (SMstate s, SMevent e, SMaction a) =>
               TMS s e a -> [s]
orderStates sm = 
    concat $ map parentsThenChildren parents
    where
        states   = Map.keys $ tms'specification sm
        children = Map.keys $ tms'childParent sm
        parents  = states \\ children
        parentsThenChildren st = st: sms'substates ((Map.!) (tms'specification sm) st)

allTransitions :: (SMstate s, SMevent e, SMaction a) =>
                    TMS s e a -> [Transition s e a]
allTransitions sm = concat $ map stateSpecToTransitions $ Map.keys spec
    where
        spec = tms'specification sm
        stateSpecToTransitions state = txspec state state [] (filteredEvents [] state)
        filteredEvents alreadyHandled st = filter ((`notElem` alreadyHandled) . fst) (Map.assocs $ fromJust $ stateTransitions sm st)
        txspec startState currentState handledEvents eventTransitionList =
            (map txItem eventTransitionList) ++ 
                txParentSpec (handledEvents ++ map fst eventTransitionList)
            where 
                txItem (ev, (ac, st)) = Transition startState ev (actionsFromTransition startState st ac) st
                txParentSpec handledEvents' = case (Map.!?) (tms'childParent sm) currentState of
                        Just parent -> txspec startState parent handledEvents' (filteredEvents handledEvents' parent)
                        Nothing -> []
                actionsFromTransition st st' ac
                            | st == st' = ac
                            | parentOf st == parentOf st' =
                                if st == parentOf st
                                then noConsecutiveActions (ac ++ entranceActions st')
                                else noConsecutiveActions (exitActions st ++ ac)
                            | otherwise = noConsecutiveActions (concat $ map exitActions $ ancestors st) ++ 
                                          ac ++ (concat $ map entranceActions $ reverse $ ancestors st')
                            where
                                parentOf state = fromMaybe state $ ((Map.!?) (tms'childParent sm) state)
                                -- |Provide list of ancestors, including self
                                ancestors state = [state] ++ (maybeToList ((Map.!?) (tms'childParent sm) state))
                                entranceActions state = sms'enter ((Map.!) spec state)
                                exitActions state = sms'exit ((Map.!) spec state)
                                noConsecutiveActions [] = []
                                noConsecutiveActions [ac] = [ac]
                                noConsecutiveActions (ac: acs@(ac':_))
                                                    | ac == ac' = noConsecutiveActions acs
                                                    | otherwise = ac: noConsecutiveActions acs

-- |Provide the transition map for a particular state
stateTransitions :: (SMstate s, SMevent e, SMaction a) =>
                    TMS s e a -> s -> Maybe (Map.Map e ([a], s))
stateTransitions sm st = sms'transitions <$> ((Map.!?) (tms'specification sm) st)

nextOperation :: (SMstate s, SMevent e, SMaction a) => TMS s e a
                    -> s -> e -> Maybe ([a],s)
nextOperation sm st ev = 
        case stateTransitions sm st of
            Just evMap -> (Map.!?) evMap ev
            Nothing    -> Nothing

transition :: (SMstate s, SMevent e, SMaction a) =>
              [Transition s e a] -> s -> e -> Maybe (Transition s e a)
transition [] _ _ = Nothing
transition (t@(Transition st ev _ _):ts) st' ev'
           | st == st' && ev == ev' = Just t
           | otherwise              = transition ts st' ev'

nextTransition :: (SMstate s, SMevent e, SMaction a) => TMS s e a -> s -> e -> Maybe s
nextTransition sm st ev = fmap snd $ nextOperation sm st ev

nextState :: (SMstate s, SMevent e, SMaction a) => TMS s e a -> s -> e -> s
nextState sm st ev = fromMaybe st $ nextTransition sm st ev

transitiveClosure :: (SMstate s, SMevent e, SMaction a) => TMS s e a -> [s]
transitiveClosure sm = allStates sm [tms'initialState sm] [tms'initialState sm]
    where
        allStates :: (SMstate s, SMevent e, SMaction a) => TMS s e a -> [s] -> [s] -> [s]
        allStates sm [] reach = reach
        allStates sm reachable@(st:sts) reach =
            let r = reachables sm st
                rs = [r' | r' <- r, r' `notElem` reach]
            in
                if null rs
                then allStates sm sts reach
                else allStates sm (sts ++ rs) (reach ++ rs)
        reachables sm st = map snd $ fromMaybe [] (Map.elems <$> (stateTransitions sm st))

unreachableStates :: (SMstate s, SMevent e, SMaction a) => TMS s e a -> [s]
unreachableStates sm =
    let rs = transitiveClosure sm
    in [r' | r' <- Map.keys $ tms'specification sm, r' `notElem` rs]

-- |Return all sequences where start state of subsequent matches end state of previous
daisyChains :: (SMstate s, SMevent e, SMaction a) => 
                [Transition s e a] -> [[Transition s e a]]
daisyChains [] = []
daisyChains (fi:fs) =
    let (chain, universe') = dc fi fs
    in chain : daisyChains universe'
    where
        dc start candidates =
            let priorEnd = tx'next start
                (noncandidates, potentials) = break ((priorEnd ==) . tx'current) candidates
            in
                if null potentials
                then ([start], noncandidates)
                else let (ch, u) = dc (head potentials) (noncandidates ++ tail potentials)
                     in (start: ch, u)

showDaisyChain :: (SMstate s, SMevent e, SMaction a) => 
                  [[Transition s e a]] -> [String]
showDaisyChain [] = []
showDaisyChain (t: ts) = [""] ++ (indent $ map show t) ++ showDaisyChain ts
