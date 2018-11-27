module FSM.TSMS where

import FSM.SMS
import qualified Data.Map.Strict as Map
import           Data.Maybe (maybeToList)

data States = S1 | S2 | S3 | S1s1 | S1s2 | S2s1 deriving (Eq, Ord, Show)
instance SMstate States where
    showDotState = show

data Events = E1 | E2 | E3 | E4 deriving (Eq, Ord, Show)
instance SMevent Events where
    showDotEvent = show

data Actions = A1 | A1x | A1e | A2 | A2x | A2e 
            | A2s1x | A1s2x | A3 | A4 
            deriving (Eq, Ord, Show)
instance SMaction Actions where
    showDotAction = show

specification :: Map.Map States (SmSpec States Events Actions)
specification = Map.fromList [
      (S1, SmSpec [A1x] [A1e] [S1s1, S1s2] 
        (Map.fromList [
              (E1, ([A1], S2))
            , (E2, ([], S2s1))
            , (E3, ([A1], S1s2))
            ]))
    , (S2, SmSpec [A2x] [A2e] [S2s1]
        (Map.fromList [
              (E1, ([A2], S1))
            ]))
    , (S2s1, SmSpec [A2s1x] [A3] [] Map.empty)
    , (S1s1, SmSpec [A2s1x] [A4] [] 
        (Map.fromList [
              (E4, ([A4], S1s2))
        ]))
    , (S1s2, SmSpec [] [] [] Map.empty)
    ]


testFsm = mkTms "TestFSM" specification S1

transitions = [
      Transition S1 E1 [A1x, A1, A2e] S2
    , Transition S1 E2 [A1x, A2e, A3] S2s1
    , Transition S1 E3 [A1] S1s2
    , Transition S2 E1 [A2x, A2, A1e] S1
    , Transition S2s1 E1 [A2s1x, A2x, A2, A1e] S1
    , Transition S1s1 E1 [A2s1x, A1x, A1, A2e] S2
    , Transition S1s1 E2 [A2s1x, A1x, A2e, A3] S2s1
    , Transition S1s1 E3 [A2s1x, A1] S1s2
    , Transition S1s1 E4 [A2s1x, A4] S1s2
    , Transition S1s2 E1 [A1x, A1, A2e] S2
    , Transition S1s2 E2 [A1x, A2e, A3] S2s1
    , Transition S1s2 E3 [A1] S1s2
    ]

validate :: Show a => (a -> Bool) -> a -> Maybe String
validate f val
         | f val = Nothing
         | otherwise = Just (show val ++ " did not validate")

testTransitions =
    map (maybeToList . (validate txDefined)) transitions
    where
        tx = allTransitions testFsm
        txDefined t = t `elem` tx