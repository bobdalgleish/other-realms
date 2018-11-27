module FSM.TableHtml where

import FSM.SMS
import FSM.Code
import Data.List (intercalate)

fsmToTable :: (SMstate s, SMevent e, SMaction a) =>
              TMS s e a -> Code
fsmToTable sm =
    table $
        caption (tms'name sm) ++
        headerRow ++
        (concat rowsByState)
    where
        table       = tagl "table style=\"border: 1px solid black;border-collapse:collapse;\"" 
        caption     = tag "caption style=\"font-size:120%;font-weight:bold\""
        headerRow   = row $ concat $ map th ["State", "Substate", "Event", "Actions", "Next State"]
        row         = tagl "tr"
        th          = tag "th style=\"border:1px solid black;padding:5px\""
        td          = tag "td style=\"border:1px solid black;padding:5px\""
        rowsByState = map (row . transitionToData) stTrans
        stateRows   = allTransitions sm
        stTrans     = concat $ map (\st -> filter ((st ==) . tx'current) stateRows) (orderStates sm)
        transitionToData (Transition st ev actions st') =
            th (show $ parentOf sm st) ++
            th (if st == parentOf sm st then "" else show st) ++
            td (show ev) ++
            td (intercalate "<br/>" $ map show actions) ++
            td (show st')
