{-# language TypeFamilies #-}
{-# language MultiParamTypeClasses #-}
module FSM.FsmTF where

class TFsm m where
    data Fsm m s e a
    apply :: (Eq s, Eq e) => m -> s -> e -> Maybe (a, s)