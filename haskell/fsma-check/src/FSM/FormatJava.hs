{-# LANGUAGE QuasiQuotes #-}
module FSM.FormatJava where

import FSM.Code
import Data.String.Interpolate

formatStatement, endStatement :: Code -> Code
formatStatement c = [head c] ++ (indent $ tail c)
endStatement c = init c ++ [(last c ++ ";")]

ifStmt :: String -> Code -> Code -> Code
ifStmt p t f = (braceGroup [i|if ( #{p} )|] t) ++ (braceGroup "else" f)

ifGuard :: String -> Code -> Code
ifGuard p t = braceGroup [i|if ( #{p} )|] t

method :: String -> Code -> Code
method signature body =
    [""] ++
    braceGroup signature body

methodOf :: String -> String -> String
methodOf t n = [i|#{t} #{n}()|]
