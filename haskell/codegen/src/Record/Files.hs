module Record.Files where

import System.IO

dumpToFile :: FilePath -> [String] -> IO ()
dumpToFile file l = writeFile file (unlines l)