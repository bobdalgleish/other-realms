module FSM.TestFsm where

import FSM.StateMachine

data TestFsm s e a = 
    TestFsm {
              test'startState :: s
            , test'event :: e
            , test'endState :: s
            , test'action :: a
            }

instance (Eq s, Eq e, Eq a) => Eq (TestFsm s e a) where
    s1 == s2 = (test'startState s1) == (test'startState s2) &&
               (test'event s1) == (test'event s2) &&
               (test'endState s1) == (test'endState s2)

computeTestSuite :: StateMachine s e a -> [TestFsm s e a]
computeTestSuite sm = map getSuite $ transitions sm
    where
        getSuite :: ((s,e), (a,s)) -> TestFsm s e a
        getSuite ((st, ev), (ac, st')) = TestFsm st ev st' ac

-- |Find all test sequences connected from start to end
daisyChain :: (Eq s) => [TestFsm s e a] -> [TestFsm s e a] -> [[TestFsm s e a]]
daisyChain [] _ = []
daisyChain (c:cs) alreadyUsed =
    let (ch, candidates, alreadyUsed') = chain c alreadyUsed cs
    in ch: daisyChain candidates alreadyUsed'
    where
        chain :: (Eq s) =>
               TestFsm s e a      -- ^Start of the chain
            -> [TestFsm s e a]      -- ^Tests already used
            -> [TestFsm s e a]      -- ^Candidate tests
            -> ([TestFsm s e a], [TestFsm s e a], [TestFsm s e a])
        chain start alu [] = ([start], [], start:alu)
        chain start alu (c1: candidates) =
            if test'endState start == test'startState c1
            then let (newChain, newCandidates, newAlreadyUsed) = chain c1 (start:alu) candidates
                 in (start: newChain, newCandidates, start:newAlreadyUsed)
            else let (newChain, newCandidates, newAlreadyUsed) = chain start alu candidates
                 in (start:newChain, c1:newCandidates, start:newAlreadyUsed)

