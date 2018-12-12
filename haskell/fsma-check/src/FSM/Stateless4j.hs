{-# LANGUAGE QuasiQuotes #-}
module FSM.Stateless4j where

import           FSM.SMS
import           FSM.Code
import           Data.String.Interpolate
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import           FSM.FormatJava

-- |Generate Java code for stateless4j library
showStateless4j :: (SMstate s, SMevent e, SMaction a) => 
                   TMS s e a -> Code
showStateless4j sm =
    method (methodOf "void" "initStateMachine")
    ([[i|StateMachineConfig<#{stateSpace}, #{eventSpace}> #{configName} = new StateMachineConfig<>();|]]
    ++ (concat $ map (formatStatement . endStatement .configureState) $ orderStates sm))
    where
        name = tms'name sm
        stateSpace = "State"
        stateName n = [i|#{stateSpace}.#{show n}|]
        eventSpace = "Event"
        eventName e = [i|#{eventSpace}.#{show e}|]
        configName = [i|#{firstToLower name}Config|]
        configureState st =
            [[i|#{configName}.configure(#{stateName st})|]] ++
            (if st /= parentOf sm st then [[i|.substateOf(#{stateName $ parentOf sm st})|]] else []) ++
            (map (\(ev, (_, st')) -> [i|.permit(#{eventName ev}, #{stateName st'})|]) (Map.assocs $ fromJust $ stateTransitions sm st))




-- | Generate test procedures, using JUnit4 notations
testStateless4j :: (SMstate s, SMevent e, SMaction a) => 
                   TMS s e a -> Code
testStateless4j sm =
    let transitions = allTransitions sm
        dc = daisyChains transitions
    in
        concat $ map normalTests (dc `zip` [1..])
        where
            normalTests (stateSequence, n) = [
                    "", "@Test"
                ] ++
                braceGroup [i|void test#{n}() throws Exception|]
                           ([startInState $ tx'current $ head stateSequence] ++
                            (concat $ map toState stateSequence)
                           )
            startInState st = [i|fsm = new StateMachine<>(#{stateName st}, #{configName});|]
            toState ss = [[i|fsm.fire(#{eventName $ tx'event ss});|]
                         , [i|assertEquals(#{stateName $ tx'next ss}, fsm.getState());|] ]
            stateSpace = "State"
            stateName n = [i|#{stateSpace}.#{show n}|]
            eventSpace = "Event"
            eventName e = [i|#{eventSpace}.#{show e}|]
            configName = [i|#{firstToLower $ tms'name sm}Config|]
    