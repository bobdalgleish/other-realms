{-# language GADTs #-}
{-# language AllowAmbiguousTypes #-}
{-# language MultiParamTypeClasses #-}
module FSM.SMS where

-- import Control.Monad.State

class (Ord s, Show s) => SMstate s
class (Ord e, Show e) => SMevent e
class (Eq a, Show a) => SMaction a

class SMS sm where
    nextOperation :: (SMstate s, SMevent e, SMaction a) => sm -> s -> e -> Maybe (a, s)
    nextTransition :: (SMstate s, SMevent e, SMaction a) => sm -> s -> e -> Maybe s
    nextState :: (SMstate s, SMevent e) => sm -> s -> e -> s

data TMS s e a where
    TMS :: (SMstate s, SMevent e, SMaction a) =>
            { states :: [s]
            , events :: [e]
            , actions :: [a]
            , initialStates :: [s]
            , transitions :: [((s,e),(a,s))]
            } -> TMS s e a


type Machine s e a = (s, TMS s e a)
type StateMachine s e a = Machine s e a -> (a, Machine s e a)

-- type StateMachine1 a = Machine s e a -> (a, Machine s e a)