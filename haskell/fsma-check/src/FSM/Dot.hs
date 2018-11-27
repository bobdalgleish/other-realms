module FSM.Dot where

import FSM.SMS

fsmToDot :: (SMstate s, SMevent e, SMaction a) =>
            TMS s e a -> Code
fsmToDot sm = braceGroup ("digraph " ++ tms'name sm)
                (concat (
                    subgraphs ++    
                    (map mapEvents (Map.keys $ tms'specification sm))))
        where
            superStates = filter (\lf -> length lf > 1) $ map (\(st, SmSpec _ _ subs _) -> st:subs)
                           $ Map.assocs (tms'specification sm)
            subgraph sg = braceGroup ("subgraph cluster" ++ (show $ head sg)) 
                            (["label=<" ++ (show $ head sg) ++ ">",
                              "style = \"rounded\""] ++ (map show sg))
            subgraphs = map subgraph superStates
            mapEvents st = concat $ map (\(ev, (_, st')) -> showDotTransition st ev st') 
                (fromMaybe [] (Map.assocs <$> (stateTransitions sm st)))
