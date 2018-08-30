{-# LANGUAGE QuasiQuotes #-}
module Record.Data where

import Data.List
import Data.Char
import Data.String.Interpolate

data Field = Field { fieldName :: String        -- ^name of the field
                   , fieldType :: String        -- ^type of the field
                   , fieldDescription :: String -- ^description of the field
                   }
                   deriving Show

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
genRecord family record = let base = baseFileName $ recordName record
                in
                    [ [i|File #{base}.h:|] ]
                    ++ fileHeader
                    ++ dupPreventer ((tail $ namespace record) ++ (fileNameFragments $ recordName record) ++ ["h"])
                    ++ (dumpIncludes $ sharedIncludes family)
                    ++ [""]
                    ++ (dumpNameSpace $ namespace record)
                    ++ ["", "class " ++ recordName record ++ "{"]
                    ++ ["private:"]
                    ++ (map declareField $ fields record)
                    ++ ["", "public:"]
                    ++ (dumpConstructor (recordName record) (fields record))
                    ++ (boilerPlate1 (recordName record))
                    ++ (dumpGetters (fields record))
                    ++ ["};"]
                    ++ (endNameSpace $ namespace record)
                    ++ ["#endif"]

baseFileName :: String -> String
baseFileName cls = map toLower $ intercalate "_" (fileNameFragments cls)

fileNameFragments :: String -> [String]
fileNameFragments [] = []
fileNameFragments s = let (pre, post) = break isUpper s
                 in case pre of
                    [] -> let r = tail post
                              (rf, rb) = break isUpper r
                          in (head post:rf) : fileNameFragments rb
                    word -> word : fileNameFragments post

include inc = [i|#include <#{inc}>|]

dumpIncludes :: [String] -> [String]
dumpIncludes i = map (\inc -> include inc) i

fileHeader = [ "/************************************************************************"
             , " *                                                                       *"
             , " *   Contractor: SED SYSTEMS, SASKATOON, SASKATCHEWAN, CANADA            *"
             , " *               (C) 2018 SED Systems, A Division of Calian Ltd.         *"
             , " *               All rights reserved.                                    *"
             , " *                                                                       *"
             , " ************************************************************************/"
             , ""
             ]

dupPreventer :: [String] -> [String]
dupPreventer s = let preventer = (intercalate "_" $ map (\n -> map toUpper n) s) ++ "_"
                 in [[i|#ifndef #{preventer}|], [i|#define #{preventer}|], ""]

dumpNameSpace :: [String] -> [String]
dumpNameSpace n = map (\namespace -> [i|namespace #{namespace} {|]) n

endNameSpace :: [String] -> [String]
endNameSpace n = "" : (map (\namespace -> [i|} /* #{namespace} */|]) $ reverse n)

declareField :: Field -> String
declareField fld = [i|    #{fieldType fld} _#{fieldName fld}; //<#{fieldDescription fld}|]

indent :: String -> String
indent s = "    " ++ s

indentN :: Int -> String
indentN n = intercalate "" $ replicate n "    "

dumpConstructor :: String -> [Field] -> [String]
dumpConstructor cls flds =  [ [i|    #{cls}();
|]
                            , [i|    #{cls}(|]
                            ] ++ 
                            dumpFormalParameters flds ++
                            [
                              [i|    );|]
                            ]

boilerPlate1 :: String -> [String]
boilerPlate1 cls = [[i|
    virtual ~#{cls}();

    #{cls}(const uint8_t* buff, std::size_t availLength, std::size_t& used );

    bool merge( const #{cls}& that );
    
    bool operate==( const #{cls}& that );
|]]

dumpFormalParameters :: [Field] -> [String]
dumpFormalParameters flds = let fieldUse = map (\f -> [i|        #{fieldType f} #{fieldName f}|]) flds
                                l = length fieldUse
                            in (map (++ ",") (init fieldUse)) ++ (drop (l-1) fieldUse)

firstToUpper :: String -> String
firstToUpper (f:r) = (if isLower f then toUpper f else f) : r

dumpGetters :: [Field] -> [String]
dumpGetters flds = map (\f -> indent $ "const " ++ fieldType f ++ "& get" ++ (firstToUpper $ fieldName f) ++ "() const;" ) flds

recordFamily = RecordFamily { recordFamilyName = "family",
                              sharedIncludes = ["string", "fstream"]
                            }
                                 
record1 = Record { recordName = "RpdLogCtrl"
                 , fields = [
                              Field "first" "char" "first character"
                            , Field "second" "int" "an int"
                            ]
                 , namespace = ["sedsystems", "gcp", "message", "rcp", "def"]
                 , includes = [
                                "message/rcp/rcp.h"
                              , "gcp/message/rcp/def/rcp_field_types.h"
                              , "docsis/common/tlv/def/tlv_complex.h"
                              ]
                 }

main = do
    putStr $ unlines $ genRecord recordFamily record1