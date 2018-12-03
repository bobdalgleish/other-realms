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

