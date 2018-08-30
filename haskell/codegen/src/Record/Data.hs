module Record.Data where

import Data.List

data Field = Field String String String
                   deriving Show

fieldName, fieldType, fieldDescription :: Field -> String
fieldName (Field n _ _) = n
fieldType (Field _ t _) = t
fieldDescription (Field _ _ d) = d

-- |Describe a data structure
data Record = Record { recordName :: String
                     , fields :: [Field]
                     , namespace :: [String]
                     , includes :: [String]
                     }
                     deriving Show

data RecordFamily = RecordFamily { recordFamilyName :: String
                                 , sharedIncludes :: [String]
                                 }
                                 deriving Show               

generateCode :: RecordFamily -> [Record] -> String
generateCode family records = show $ map (\r -> unlines $ genRecord family r) records
                              
genRecord :: RecordFamily -> Record -> [String]
genRecord family record = [ "File " ++ recordName record ++ ".h:" ]
                    ++ fileHeader
                    ++ (dumpIncludes $ sharedIncludes family)
                    ++ (dumpNameSpace $ namespace record)
                    ++ ["class " ++ recordName record ++ "{"]
                    ++ ["private:"]
                    ++ (map declareField $ fields record)
{-}                    , "public:"
                    , dumpConstructor (recordName record) (fields record)
-}
                    ++ ["};"]
                    ++ (endNameSpace $ namespace record)


dumpIncludes :: [String] -> [String]
dumpIncludes i = map (\include -> "#include <" ++ include ++ ">") i

fileHeader = ["/** first line", " * Copyright 2018", " */"]

dumpNameSpace :: [String] -> [String]
dumpNameSpace n = map (\namespace -> "namespace " ++ namespace ++ " {") n

endNameSpace :: [String] -> [String]
endNameSpace n = map (\namespace -> "} /* " ++ namespace ++ " */") n

declareField :: Field -> String
declareField fld = "    " ++ fieldType fld ++ " " ++ fieldName fld ++ "; //<" ++ fieldDescription fld

recordFamily = RecordFamily { recordFamilyName = "family",
                              sharedIncludes = ["string", "fstream"]
                            }
                                 
record1 = Record { recordName = "LogCtrl"
                 , fields = [
                     Field "first" "char" "first character"
                 ]
                 , namespace = ["sedssystems", "gcp"]
                 , includes = ["gcp/ccap/def/rcp_field_types.h"]
                 }