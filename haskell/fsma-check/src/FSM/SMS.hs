{-# language GADTs #-}
{-# language AllowAmbiguousTypes #-}
{-# language MultiParamTypeClasses #-}
module FSM.SMS where

import           Data.Maybe (fromMaybe)
import           Data.List (nub, break)
import qualified Data.Map.Strict as Map

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

-- |Handle lines of code
type Code = [String]

-- |Indent lines of code by 4-spaces
indent :: Code -> Code
indent c = map ("    " ++) c

braceGroup :: String -> Code -> Code
braceGroup pre code = [pre ++ " {"] ++ indent code ++ ["}"]
    
showDotTransition :: (SMstate s, SMevent e) => s -> e -> s -> Code
showDotTransition st ev  st' = [showDotState st ++ " -> " ++ showDotState st' ++ 
    "[label=<" ++ showDotEvent ev ++ ">];"]

fsmToDot :: (SMstate s, SMevent e, SMaction a) =>
            TMS s e a -> Code
fsmToDot sm = braceGroup ("digraph " ++ tms'name sm)
                (concat $ map mapEvents (Map.keys $ tms'specification sm))
        where
            mapEvents st = concat $ map (\(ev, (_, st')) -> showDotTransition st ev st') 
                                (fromMaybe [] (Map.assocs <$> (stateTransitions sm st)))

instance (SMstate s, SMevent e, SMaction a) => Show (Transition s e a) where
    show = showTransition

data SmSpec s e a where
    SmSpec :: (SMstate s, SMevent e, SMaction a) =>
               {
                 sms'exit  :: [a]                       -- ^Actions to perform on exit
               , sms'enter :: [a]                       -- ^Actions to perform on entry
               , sms'transitions :: Map.Map e ([a], s)  -- ^State transitions
               } -> SmSpec s e a

allTransitions :: (SMstate s, SMevent e, SMaction a) =>
                     Map.Map s (SmSpec s e a) -> [Transition s e a]
allTransitions spec = stateSpecToTransitions (Map.keys spec)
    where
        stateSpecToTransitions [] = []
        stateSpecToTransitions (state:sps) = 
            (map (\(ev, (ac, st)) -> Transition state ev (actionsFromTransition state st ac) st) (Map.assocs (sms'transitions ((Map.!) spec state)))) 
                ++ stateSpecToTransitions sps
        actionsFromTransition st st' ac
                              | st == st' = ac
                              | otherwise = noConsecutiveActions ((sms'exit ((Map.!) spec st)) ++ ac ++ (sms'enter ((Map.!) spec st')))
                              where
                                noConsecutiveActions [] = []
                                noConsecutiveActions [ac] = [ac]
                                noConsecutiveActions (ac:ac':acs)
                                                    | ac == ac' = noConsecutiveActions (ac:acs)
                                                    | otherwise = ac: noConsecutiveActions (ac':acs)

data TMS s e a where
    TMS :: (SMstate s, SMevent e, SMaction a) =>
            { tms'name          :: String                   -- ^Name of the state machine
            , tms'initialState  :: s                        -- ^Initial state
            , tms'specification :: Map.Map s (SmSpec s e a) -- ^Transition specification
            } -> TMS s e a

mkTms :: (SMstate s, SMevent e, SMaction a) =>
         String -> Map.Map s (SmSpec s e a) -> s -> TMS s e a
mkTms name spec initial = TMS {
                           tms'name          = name
                         , tms'initialState  = initial
                         , tms'specification = spec
                         }

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

-- |Generate Haskell version of state machine
showHaskell :: (SMstate s, SMevent e, SMaction a) => 
               TMS s e a -> Code
showHaskell sm = fsmToHaskellStates ++
                 fsmToHaskellEvents ++
                 fsmToHaskellActions ++
                 fsmToHaskellTransitions
    where
        smName = tms'name sm
        smStates = Map.keys $ tms'specification sm
        bareTransitions = map sms'transitions $ Map.elems $ tms'specification sm
        smEvents = nub $ concat $ map Map.keys bareTransitions
        smTransitions = allTransitions $ tms'specification sm
        smActions = nub $ concat $ map tx'actions smTransitions
        fsmToHaskellStates = ["data " ++ stateName ++ " ="] ++
                             indent ((map (("| "++) . show) smStates) ++
                                     ["deriving (Show, Eq)"])
        fsmToHaskellEvents = ["data " ++ eventName ++ " =" ] ++
                             indent ( (map (("| "++). show) smEvents) ++
                                      ["deriving (Show, Eq)"])
        fsmToHaskellActions = ["data " ++ actionName ++ " ="] ++
                              indent ( (map (("| "++) . show) smActions) ++
                                       ["deriving (Show, Eq)"])
        fsmToHaskellTransitions = ["transition :: " ++ 
                                    stateName ++ " -> " ++ 
                                    eventName ++ " -> ([" ++ 
                                    actionName ++ "], " ++ 
                                    stateName ++ ")" ] ++
                                  (map asHaskellTransition smTransitions) ++
                                  ["transition st _ = ([], st)"]
        asHaskellTransition tr = "transition " ++ 
                                    (show $ tx'current tr) ++
                                    " " ++ (show $ tx'event tr) ++
                                    " = (" ++ (show $ tx'actions tr) ++
                                    ", " ++ (show $ tx'next tr) ++ ")"
        stateName  = smName ++ "'State"
        eventName  = smName ++ "'Event"
        actionName = smName ++ "'Action"