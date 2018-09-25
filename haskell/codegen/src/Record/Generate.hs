{-# LANGUAGE QuasiQuotes #-}
module Record.Generate where

import Record.Data
import Data.List
import Data.Char
import Data.String.Interpolate

testString = [i|This is a test string|]

generateCode :: RecordFamily -> [Record] -> String
generateCode family records = show $ map (\r -> unlines $ genHeader family r) records
                              
genHeader :: RecordFamily -> Record -> [String]
genHeader family record = let base = baseFileName $ recordName record
                in
                    fileHeader
                    ++ dupPreventer ((tail $ namespace record) ++ (fileNameFragments $ recordName record) ++ ["h"])
                    ++ (dumpIncludes $ sharedIncludes family)
                    ++ [""]
                    ++ (doNameSpace (namespace record) (
                        ["", "class " ++ recordName record ++ ": public docsis::common::tlv::def::TlvComplex"]
                        ++ ["{", "public:"]
                        ++ (dumpConstructor record)
                        ++ (boilerPlate1 (recordName record))
                        ++ (dumpGetters record (fields record))
                        ++ [(boilerPlate2 (recordName record))]
                        ++ ["", "private:"]
                        ++ (map (declareField record) $ fields record)
                        ++ ["};"]
                    ))
                    ++ ["#endif"]

genBody :: RecordFamily -> Record -> [String]
genBody family record =
    let includeFile = headerFileName record
    in
        fileHeader
        ++ [incl $ intercalate "/" (srcPath record ++ [includeFile])]
        ++ [""]
        ++ (doNameSpace (namespace record) 
            ( (dumpRealConstructor record) ++
              dumpGetTlvType record ++
              dumpGetterImps record (fields record) ++
              dumpGetNumOptionalTlvs record ++
              dumpGetOptionalTlv record ++
              constructFromBuffer record
            )
            )
        
genTest :: RecordFamily -> Record -> [String]
genTest family record =
        fileHeader
        ++ [""]
        ++ (dumpIncludes [
                          "gtest/gtest.h"
                        , "gmock/gmock.h"
                        , "core/log/debug_logger_store.h"
                        , "core/log/logger_macros.h"
                        , "docsis/common/tlv/def/parse_exception.h"
                        , "gcp/message/rcp/parse/rcp_operation_factory_impl.h"
                        , "gcp/message/rcp/def/rcp_top_level_message.h"
                        , "gcp_rcp_msg_test_utils.h"
                        ])
        ++ [
             useNameSpace ["sedsystems", "gcp"]
           , useNameSpace ["sedsystems", "docsis", "common", "tlv", "def"]
           , useNameSpace ["sedsystems", "gcp", "message", "rcp", "def"]
           , useNameSpace ["testing"]
           ]
        ++ doNameSpace ["MockUsscMessage"] (
            defineTestClass record
        )

baseFileName :: String -> String
baseFileName cls = map toLower $ intercalate "_" (fileNameFragments cls)

headerFileName :: Record -> String
headerFileName r = baseFileName $ recordName r ++ ".h"

classFileName :: Record -> String
classFileName r = baseFileName $ recordName r ++ ".cpp"

testFileName :: Record -> String
testFileName r = baseFileName $ recordName r ++ "_test.cpp"

srcPath :: Record -> [String]
srcPath r = (tail $ namespace r)

fileNameFragments :: String -> [String]
fileNameFragments [] = []
fileNameFragments s = let (pre, post) = break isUpper s
                 in case pre of
                    [] -> let r = tail post
                              (rf, rb) = break isUpper r
                          in (head post:rf) : fileNameFragments rb
                    word -> word : fileNameFragments post

firstToUpper :: String -> String
firstToUpper (f:r) = (if isLower f then toUpper f else f) : r

upperSnakeCase :: String -> String
upperSnakeCase s = map toUpper $ intercalate "_" $ fileNameFragments s

incl :: String -> String
incl inc = [i|#include <#{inc}>|]

dumpIncludes :: [String] -> [String]
dumpIncludes i = map incl i

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

useNameSpace :: [String] -> String
useNameSpace n = 
    let ns = intercalate "::" n
    in  [i|using namespace #{concat n};|]

declareField :: Record -> Field -> String
declareField r fld = [i|    boost::optional<#{showField r (fieldType fld)}> _#{fieldName fld};|]

indentN :: Int -> String
indentN n = concat $ replicate n "    "

indentLines :: Int -> [String] -> [String]
indentLines n l = map (indentN n ++) l

defineTestClass :: Record -> [String]
defineTestClass r =
    [
      [i|class #{recordName r}Test: public Testing|]
    , "{"
    , "};"
    ]

dumpConstructor :: Record -> [String]
dumpConstructor r =  [ [i|    #{recordName r}();|]
                     , ""
                     , [i|    #{recordName r}(|]
                     ] ++ 
                     (indentLines 2 $ dumpFormalParameters r) ++
                     [
                       [i|    );|]
                     ]
dumpRealConstructor :: Record -> [String]
dumpRealConstructor r =
    [
        [i|#{recordName r}::#{recordName r}()|]
        , "{"
        , "}"
        , ""
        , [i|#{recordName r}::~#{recordName r}()|]
        , "{"
        , "}"
        , ""
        , [i|#{recordName r}::#{recordName r}(|]
    ] ++
    indentLines 1 (dumpFormalParameters r) ++
    ["    )", "    : TlvComplex( getTlvType() )"] ++
    (braces (map (\field -> [i|buildOptional( #{showTlvField r $ fieldType field}, #{fieldName field}, _#{fieldName field} );|]) (fields r) ++
        [ "updateValueLength();"]) )

constructFromBuffer :: Record -> [String]
constructFromBuffer r = [
    "",
    "/** Construct from a buffer with any length */",
    [i|#{recordName r}::#{recordName r}(|],
    "    const uint8_t* buff,",
    "    std::size_t availLength,",
    "    std::size_t& used )",
    "    : TlvComplex( buff, availLength )"
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
            , [i|#{baseTlv r} tlv_t = static_cast<#{baseTlv r}>( *(buff + used) );|]
            , [i|bool known = true;|]
            ] ++
            (doSwitch "tlv_t" (map genCase $ fields r)
                    [
                        "discardUnknown( buff + used, availLen, dataUsed );"
                      , "known = false;"
                      , "break;"
                    ] ) ++
            [ "if (known)"
            , "{"
            , "    foundTlv( tlv_t );"
            , "}"
            , "used += dataUsed;"
            , "availLen -= dataUsed;"
            ]
            ) ++
        [
            "if (availLen != 0)"
          , "{"
          , "    throw ParseException("
          , [i|       "Unused/overused length inside #{baseTlv r}" );|]
          , "}"
          , "updateValueLength();"
        ]
    )
    where
        genCase :: Field -> (String, [String])
        genCase f = (showTlvField r $ fieldType f,
                     [     [i|_#{fieldName f} = #{showField r (fieldType f)}( buff + used, availLen, dataUsed );|]
                         , "break;"
                     ]
                    )

braces :: [String] -> [String]
braces bl = ["{"] ++ (indentLines 1 bl) ++ ["}"]

boilerPlate1 :: String -> [String]
boilerPlate1 cls = [[i|
    virtual ~#{cls}();

    #{cls}(const uint8_t* buff, std::size_t availLength, std::size_t& used );
|]]

boilerPlate2 :: String -> String
boilerPlate2 cls = [i|    uint8_t getTlvType();
protected:
    /**
     * Sub-classes indicate how many TLVs are in the value portion
     * of the complex TLV
     * @return count of all optional TLVs
     */
    std::size_t getNumOptionalTlvs() const override;

    /**
     * @param index - 0-based index of TLV
     * @return reference to indexed TLV
     */
    boost::optional<const TlvBase_t<uint16_t>&> getOptionalTlv(
        std::size_t index ) const override;|]

dumpFormalParameters :: Record -> [String]
dumpFormalParameters r = let fieldUse = map (\f -> [i|#{optOf $ showValField r $ fieldType f} #{fieldName f}|]) (fields r)
                             l = length fieldUse
                         in (map (++ ",") (init fieldUse)) ++ (drop (l-1) fieldUse)

dumpGetters :: Record -> [Field] -> [String]
dumpGetters r flds = map (\f -> [i|    const #{optOf $ showField r $ fieldType f}& get#{firstToUpper $ fieldName f}() const;|] ) flds

dumpGetterImps :: Record -> [Field] -> [String]
dumpGetterImps _ [] = []
dumpGetterImps r (f: fs) = [
                           ""
                         , [i|const #{optOf $ showField r $ fieldType f}& #{recordName r}::get#{firstToUpper $ fieldName f}() const|]
                         ] ++
                         braces [[i|return _#{fieldName f};|]] ++
                         dumpGetterImps r fs
                        
dumpGetTlvType :: Record -> [String]
dumpGetTlvType r =
    [
      ""
    , [i|uint8_t #{recordName r}::getTlvType()|]
    ] ++
    braces [[i|return static_cast<uint8_t>( BaseRpdInfoTLV_t::#{upperSnakeCase $ recordName r};|]]

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
    braces (
            doSwitch "index" (map genCase $ fields r) [[i|    throw std::range_error( "Index out of range for #{baseTlv r}" );|]]
        )
    where
        genCase :: Field -> (String, [String])
        genCase f = (showTlvField r $ fieldType f, [[i|return getOptBase16( _#{fieldName f} );|]])

doSwitch :: String -> [(String, [String])] -> [String] -> [String]
doSwitch key cases dflt =
    [
        [i|switch ( #{key} )|]
      , "{"
    ] ++
    handleCases cases ++
    [ "default:"] ++
    indentLines 1 dflt ++
    [ "}" ]

handleCases :: [(String, [String])] -> [String]
handleCases [] = []
handleCases ((v, s):vss) = [ [i|case #{v}:|] ] ++ (indentLines 1 s) ++ handleCases vss