{-# LANGUAGE QuasiQuotes #-}
module FSM.Code where

import           Data.Char
-- import Debug.Trace
import Data.String.Interpolate

-- |Handle lines of code
type Code = [String]

-- |Indent lines of code by 4-spaces
indent :: Code -> Code
indent c = map ("    " ++) c

-- |Surround a block of code with braces, indenting
braceGroup :: String -> Code -> Code
braceGroup pre code = 
    let opening  = if pre == "" then ["{"] else [pre ++ " {"]
    in opening ++ indent code ++ ["}"]
    
wrap :: String -> String -> Code
wrap tag stuff = [ [i|<#{tag}>#{stuff}</#{tagOf tag}>|] ]
wrapl :: String -> Code -> Code
wrapl tag stuff = [[i|<#{tag}>|]] ++ (indent stuff) ++ [[i|</#{tagOf tag}>|]]

tag :: String -> String -> Code
tag t content = [[i|<#{t}>#{content}</#{tagOf t}>|]]

tagl :: String -> Code -> Code
tagl t content = [[i|<#{t}>|]] ++
                 (indent content) ++
                 [[i|</#{tagOf t}>|]]

-- |Extract the tag from the tag and its attributes
tagOf :: String -> String
tagOf tag = head $ words tag

-- |Convert initial character to upper case
firstToUpper :: String -> String
firstToUpper (f:r) = (if isLower f then toUpper f else f) : r

-- |Convert initial character to lower case
firstToLower :: String -> String
firstToLower (f:r) = (if isUpper f then toLower f else f) : r
