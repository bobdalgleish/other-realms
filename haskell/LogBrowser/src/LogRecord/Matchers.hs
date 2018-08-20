module LogRecord.Matchers where

import Data.List (break)

-- |Split on the last matching character
breakEnd :: (Char -> Bool) -> String -> (String, String)
breakEnd c s = let parts = splitBy c s
                   len   = length parts
                   back  = last parts
                   front = if len > 1 then (take (length s - 1 - length back) s) else []
               in (front, back)

-- |Split on the matching character
splitBy :: (Char -> Bool) -> String -> [String]
splitBy _ [] = []
splitBy p s = let (front, back) = break p s
              in front : if length back > 1 then (splitBy p (tail back)) else []
