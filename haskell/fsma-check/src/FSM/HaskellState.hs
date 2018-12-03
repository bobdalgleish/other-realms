{-# LANGUAGE QuasiQuotes #-}
module FSM.HaskellState where

import           FSM.SMS
import           FSM.Code
import           Data.String.Interpolate
import qualified Data.Map.Strict as Map
import           Data.List (nub)

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
        smTransitions = allTransitions sm
        smActions = nub $ concat $ map tx'actions smTransitions
        fsmToHaskellStates = [[i|data #{stateName} =|]] ++
                             indent ((map (("| "++) . show) smStates) ++
                                     [[i|deriving (Show, Eq)|]])
        fsmToHaskellEvents = [[i|data #{eventName} =|] ] ++
                             indent ( (map (("| "++). show) smEvents) ++
                             [[i|deriving (Show, Eq)|]])
        fsmToHaskellActions = [[i|data #{actionName} =|]] ++
                              indent ( (map (("| "++) . show) smActions) ++
                              [[i|deriving (Show, Eq)|]])
        fsmToHaskellTransitions = [[i|transition :: #{stateName} -> #{eventName} -> ([#{actionName}], #{stateName})|]] ++
                                  (map asHaskellTransition smTransitions) ++
                                  [[i|transition st _ = ([], st)|]]
        asHaskellTransition tr = "transition " ++ 
                                    (show $ tx'current tr) ++
                                    " " ++ (show $ tx'event tr) ++
                                    " = (" ++ (show $ tx'actions tr) ++
                                    ", " ++ (show $ tx'next tr) ++ ")"
        stateName  = smName ++ "'State"
        eventName  = smName ++ "'Event"
        actionName = smName ++ "'Action"
