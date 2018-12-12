module FSM.TableHtml where

import FSM.SMS
import FSM.Code
import Data.List (intercalate)

-- | Generate a table from FSM specification
-- Table has columns for state and sub-state, events and actions
-- Provide the style elements before the table
-- TODO Combine columns with shared start state
fsmToTable :: (SMstate s, SMevent e, SMaction a) =>
              TMS s e a -> Code
fsmToTable sm =
    table $
        caption (tms'name sm) ++
        headerRow ++
        (concat rowsByState)
    where
        table       = tagl "table" 
        caption     = tag "caption"
        headerRow   = row $ concat $ map th ["State", "Substate", "Event", "Actions", "Next State"]
        row         = tagl "tr"
        th          = tag "th"
        td          = tag "td"
        rowsByState = map (row . transitionToData) stTrans
        stateRows   = allTransitions sm
        stTrans     = concat $ map (\st -> filter ((st ==) . tx'current) stateRows) (orderStates sm)
        transitionToData (Transition st ev actions st') =
            td (show $ parentOf sm st) ++
            td (if st == parentOf sm st then "" else show st) ++
            td (show ev) ++
            td (intercalate "<br/>" $ map show actions) ++
            td (show st')

tableStyle :: Code
tableStyle =
    wrapl "style" [
        "table, th, td {border: 1px solid black;}",
        "table {border-collapse: collapse;}",
        "caption {font-size:120%;font-weight:bold}",
        "th, td {padding:5px;}"
    ]
