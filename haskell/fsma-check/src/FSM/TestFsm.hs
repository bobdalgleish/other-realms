module FSM.TestFsm where

import FSM.StateMachine

data TestFsm s e a = 
    TestFsm {
              test'startState :: s
            , test'event :: e
            , test'endState :: s
            , test'action :: a
            }

computeTestSuite :: StateMachine s e a -> [TestFsm s e a]
computeTestSuite sm = map getSuite $ transitions sm
    where
        getSuite :: ((s,e), (a,s)) -> TestFsm s e a
        getSuite ((st, ev), (ac, st')) = TestFsm st ev st' ac
