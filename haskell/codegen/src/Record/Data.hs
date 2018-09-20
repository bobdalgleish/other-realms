{-# LANGUAGE QuasiQuotes #-}
module Record.Data where

import Data.List
import Data.Char
import Data.String.Interpolate

data TlvType = TlvType {
      rootOf :: String
    , index :: String
    , tlvEnum :: String
}

instance Show TlvType
    where show a = rootOf a ++ index a

showField, showOptField, showTlvField, showValField :: TlvType -> String
showField t = [i|#{rootOf t}#{index t}Type|]
showOptField t = optOf $ showField t
showTlvField t = [i|#{rootOf t}TLV_t::#{tlvEnum t}|]
showValField t = [i|#{showField t}Type::VALUE_TYPE|]

optOf :: String -> String
optOf s = [i|boost::optional<#{s}>|]

data Field = Field { fieldName :: String        -- ^name of the field
                   , fieldType :: TlvType        -- ^type of the field
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
generateCode family records = show $ map (\r -> unlines $ genHeader family r) records
                              
genHeader :: RecordFamily -> Record -> [String]
genHeader family record = let base = baseFileName $ recordName record
                in
                    fileHeader
                    ++ dupPreventer ((tail $ namespace record) ++ (fileNameFragments $ recordName record) ++ ["h"])
                    ++ (dumpIncludes $ sharedIncludes family)
                    ++ [""]
                    ++ (dumpNameSpace $ namespace record)
                    ++ ["", "class " ++ recordName record ++ ": public docsis::common::tlv::def::TlvComplex"]
                    ++ ["{", "public:"]
                    ++ (dumpConstructor (recordName record) (fields record))
                    ++ (boilerPlate1 (recordName record))
                    ++ (dumpGetters (fields record))
                    ++ [(boilerPlate2 (recordName record))]
                    ++ ["", "private:"]
                    ++ (map declareField $ fields record)
                    ++ ["};"]
                    ++ (endNameSpace $ namespace record)
                    ++ ["#endif"]

genBody :: RecordFamily -> Record -> [String]
genBody family record =
    let includeFile = headerFileName record
    in
        fileHeader
        ++ [""]
        ++ [include includeFile]
        ++ [""]
        ++ (doNameSpace (namespace record) 
            ( (dumpRealConstructor record) ++
              dumpGetterImps record (fields record) ++
              dumpGetNumOptionalTlvs record ++
              dumpGetOptionalTlv record ++
              constructFromBuffer record
            )
            )
        

baseFileName :: String -> String
baseFileName cls = map toLower $ intercalate "_" (fileNameFragments cls)

headerFileName :: Record -> String
headerFileName r = baseFileName $ recordName r ++ ".h"

classFileName :: Record -> String
classFileName r = baseFileName $ recordName r ++ ".cpp"

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

copyrightYear = 2018

fileHeader = [ "/************************************************************************"
             , " *                                                                       *"
             , " *   Contractor: SED SYSTEMS, SASKATOON, SASKATCHEWAN, CANADA            *"
             , [i| *               (C) #{copyrightYear} SED Systems, A Division of Calian Ltd.         *|]
             , " *               All rights reserved.                                    *"
             , " *                                                                       *"
             , " ************************************************************************/"
             , ""
             ]

dupPreventer :: [String] -> [String]
dupPreventer s = let preventer = (intercalate "_" $ map (\n -> map toUpper n) s) ++ "_"
                 in [[i|#ifndef #{preventer}|], [i|#define #{preventer}|], ""]

dumpNameSpace :: [String] -> [String]
dumpNameSpace n = map (\namespace -> [i|namespace #{namespace}
{|]) n
doNameSpace :: [String] -> [String] -> [String]
doNameSpace [] body = body
doNameSpace (n: ns) body = [ [i|namespace #{n}|], "{"] ++
                    doNameSpace ns body ++
                    [ [i|} /** #{n} */|]]

endNameSpace :: [String] -> [String]
endNameSpace n = "" : (map (\namespace -> [i|} /* #{namespace} */|]) $ reverse n)

declareField :: Field -> String
declareField fld = [i|    boost::optional<#{fieldType fld}> _#{fieldName fld}; //<#{fieldDescription fld}|]

indent :: String -> String
indent s = "    " ++ s

indentN :: Int -> String
indentN n = intercalate "" $ replicate n "    "

indentBlock :: Int -> [String] -> [String]
indentBlock n l = map (indentN n ++) l

dumpConstructor :: String -> [Field] -> [String]
dumpConstructor cls flds =  [ [i|    #{cls}();
|]
                            , [i|    #{cls}(|]
                            ] ++ 
                            (indentBlock 2 $ dumpFormalParameters flds) ++
                            [
                              [i|    );|]
                            ]
dumpRealConstructor :: Record -> [String]
dumpRealConstructor r =
    [
        [i|#{recordName r}()|]
        , "{"
        , "}"
        , ""
        , [i|#{recordName r}::~#{recordName r}()|]
        , "{"
        , "}"
        , ""
        , [i|#{recordName r}::#{recordName r}(|]
    ] ++
    indentBlock 1 (dumpFormalParameters $ fields r) ++
    [")", "        : TlvComplex( getTlvType() )"] ++
    (braces (map (\field -> [i|buildOptional( #{showTlvField $ fieldType field}, #{fieldName field}, _#{fieldName field} );|]) (fields r) ++
        [ "updateValueLength();"]) )

constructFromBuffer :: Record -> [String]
constructFromBuffer r = [
    "",
    "/** Construct from a buffer with any length */",
    [i|#{recordName r}::#{recordName r}(|],
    "    const uint8_t* buff,",
    "    std::size_t availLength,",
    "    std::size_t& used )"
    ] ++
    braces ( [
        "if (getType() != getTlvType())",
        "{",
        "    throw ParseException(",
        [i|        "Unexpected message type given to #{recordName r}" );|],
        "}",
        "uint16_t availLen = getLength();",
        "used = getBaseLength();",
        "while ( availLen > 0 )"
        ] ++
        braces (
            [ "std::size_t dataUsed = 0;"
            , [i|#{|]
                "switch"])
    )

braces :: [String] -> [String]
braces bl = ["{"] ++ (indentBlock 1 bl) ++ ["}"]


boilerPlate1 :: String -> [String]
boilerPlate1 cls = [[i|
    virtual ~#{cls}();

    #{cls}(const uint8_t* buff, std::size_t availLength, std::size_t& used );

    bool merge( const #{cls}& that );
    
    bool operate==( const #{cls}& that );
|]]

boilerPlate2 :: String -> String
boilerPlate2 cls = [i|    uint8_t getTlvType();
protected:
    /**
     * Sub-classes indicate how many TLVs are in the value portion
     * of the complex TLV
     * @return count of all optional TLVs
     */
    std::size_t getNumOptionalTlvs() const;

    /**
     * @param index - 0-based index of TLV
     * @return reference to indexed TLV
     */
    boost::optional<const TlvBase_t<uint16_t>&> getOptionalTlv(
        std::size_t index ) const;
|]

dumpFormalParameters :: [Field] -> [String]
dumpFormalParameters flds = let fieldUse = map (\f -> [i|#{optOf $ showValField $ fieldType f} #{fieldName f}|]) flds
                                l = length fieldUse
                            in (map (++ ",") (init fieldUse)) ++ (drop (l-1) fieldUse)

firstToUpper :: String -> String
firstToUpper (f:r) = (if isLower f then toUpper f else f) : r

dumpGetters :: [Field] -> [String]
dumpGetters flds = map (\f -> [i|    const #{optOf $ showField $ fieldType f}& get#{firstToUpper $ fieldName f}() const;|] ) flds

dumpGetterImps :: Record -> [Field] -> [String]
dumpGetterImps _ [] = []
dumpGetterImps r (f: fs) = [
                           ""
                         , [i|const #{optOf $ showField $ fieldType f}& #{recordName r}::get#{firstToUpper $ fieldName f}() const|]
                         ] ++
                         braces [[i|return _#{fieldName f};|]] ++
                         dumpGetterImps r fs
                        
dumpGetNumOptionalTlvs :: Record -> [String]
dumpGetNumOptionalTlvs r = [
                             ""
                           , [i|std::size_t #{recordName r}::getNumOptionalTlvs() const|]
                           ] ++
                           braces [[i|return #{length $ fields r};|]]

dumpGetOptionalTlv :: Record -> [String]
dumpGetOptionalTlv r =
    [ ""
    , [i|#{optOf "const TlvBase_t<uint16_t>&"} #{recordName r}::getOptionalTlv( std::size_t index ) const|]
    ] ++
    braces ([ [i|switch ( index )|], "{"] ++
        (map (\f -> [i|case #{showTlvField $ fieldType f}: return getOptBase16( _#{fieldName f} );|]) (fields r)) ++
        [[i|default:|],
        [i|    throw std::range_error( "Index out of range for #{recordName r}TLV_t" );|],
        "}"]
        )