{-# LANGUAGE MultiParamTypeClasses #-}
module Fsm.Fsm1 where

import Fsm.StateMachine
import qualified Data.Set as Set

data FSM = 
           Discovery
         | Active
         | ActiveAlone
         | Standby
         deriving (Eq, Ord, Show)

instance State FSM where
    initialState = Discovery
    terminalStates = []
    allStates = [Discovery, Active, ActiveAlone, Standby]
        
data Tr = Timeout
        | HeartBeatPrime
        | HeartBeatSecond
        | HeartBeatOos
        | HeartBeatCommand
        | GoStandby
        deriving (Eq, Ord, Show)

instance MachineEvent Tr where
    allEvents = [
                  Timeout
                , HeartBeatPrime
                , HeartBeatSecond
                , HeartBeatOos
                , HeartBeatCommand
                , GoStandby
                ]

instance StateMachine FSM Tr where
    nextTransition f e = lookup (f, e) transitions
    nextState f e = case nextTransition f e of
                     Just nd -> nd
                     Nothing -> f

data FsmAction = Action0 | Action1

type Stimulus = (FSM, Tr)

transitions :: [(Stimulus, FSM)]
transitions =   
    [
      ((Discovery, Timeout),          ActiveAlone)
    , ((Discovery, HeartBeatPrime),   Active)
    , ((Discovery, HeartBeatSecond),  Standby)
    , ((Discovery, HeartBeatCommand), Active)
    , ((Active,    HeartBeatOos),     Standby)
    , ((Active,    Timeout),          ActiveAlone)
    , ((Active,    GoStandby),        Standby)
    , ((Standby,   HeartBeatCommand), Active)
    ]

closureFsm :: [FSM]
closureFsm = transitiveClosure (allEvents :: [Tr]) [(initialState :: FSM)]

-- |Property: all states are reachable from the initial state
allReachable :: Bool
allReachable = null unreachableStates

unreachableStates :: [FSM]
unreachableStates =
    let reachables = closureFsm
    in [s | s <- allStates, not (s `elem` reachables)]

-- testState :: [Stimulus] -> FSM
data Test = Test {
                   testName :: String
                 , startingState :: FSM
                 , action :: Tr
                 , endingState :: FSM
                 , postConditions :: String
                 }
                 deriving (Eq, Show)

testState :: FSM -> Tr -> Test
testState st ev =
    let name = "test" ++ show st ++ "Via" ++ show ev
        next = nextState st ev
    in
        Test name st ev next "?"

allTestStates :: [Test]
allTestStates =
    [testState st ev | st <- allStates, ev <- allEvents]

testStartStates :: [(FSM, [Test])]
testStartStates =
    let allTests = allTestStates
    in [ (st, filterStart st allTests) | st <- allStates]
    where
        filterStart :: FSM -> [Test] -> [Test]
        filterStart st allTests = [tst | tst <- allTests, st == startingState tst]

-- |Starting from this test, find a chain of tests that are not in the done list
groupFromHead :: Test -> [Test] -> [Test]
groupFromHead t done =
    let vetted = filterOld (commonStartToEnd t testStartStates) done
    in case vetted of
        [] -> []
        (ht:_) -> ht: groupFromHead ht (ht:done)

commonStartToEnd :: Test -> [(FSM, [Test])] -> [Test]
commonStartToEnd t m = case lookup (endingState t) m of
                        Just listOfTests -> listOfTests
                        Nothing -> []

filterOld :: [Test] -> [Test] -> [Test]
filterOld candidates done =
    [c | c <- candidates, not (c `elem` done)]

-- |Generate all sequences of tests
generateAllTestSequences :: [Test] -> [Test] -> [[Test]]
generateAllTestSequences [] _ = []
generateAllTestSequences (t:ts) done =
    let seq = t: groupFromHead t (t: done)
        newDone = done ++ seq
    in seq: generateAllTestSequences (filterOld ts newDone) newDone

generateCode :: Int -> [Test] -> [String]
generateCode n tests =
    let (t1:_) = tests
        st = startingState t1
    in
        ["",
        "public void testBulkStateTransitions" ++ show n ++ "() throws Exception {",
        "    ensureInitialState" ++ (show st) ++ "();"] ++
        concat (map generateTest tests) ++
        ["}"]

generateTest :: Test -> [String]
generateTest t =
    [
      "    // " ++ testName t
    , "    stimulate" ++ (show $ action t) ++ "();"
    , "    testState" ++ (show $ endingState t) ++ "()"
    ]

generateCodes :: [[Test]] -> [String]
generateCodes t = generateTestProcedures (t `zip` [1..])
    where
        generateTestProcedures :: [([Test], Int)] -> [String]
        generateTestProcedures [] = []
        generateTestProcedures ((t, n):ts) = generateCode n t ++ generateTestProcedures ts

generate :: [String]
generate =
    let testSequences = generateAllTestSequences allTestStates []
    in generateCodes testSequences
