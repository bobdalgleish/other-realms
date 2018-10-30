module FSM.Stateful where

import Control.Monad.State

data Event
data St
data Action

data FSM = 

applyInput :: Event -> FSM -> (Action, FSM)