{-# LANGUAGE QuasiQuotes #-}
module Record.Generate where

import Record.Data
import Data.List
import Data.Char
import Data.String.Interpolate

import System.IO

codegen :: DataRecord -> IO ()
codegen dr = do
    writeFile (headerFileName dr) (unlines $ genHeader dr)
    writeFile (classFileName dr) (unlines $ genBody dr)
    writeFile (testFileName dr) (unlines $ genTest dr)
           
genHeader :: DataRecord -> [String]
genHeader dr = 
    let base = baseFileName dr
    in
        fileHeader
        ++ dupPreventer (getSrcPath dr ++ [base] ++ ["h"])
        ++ (dumpIncludes $ includes dr)
        ++ [""]
        ++ (doNameSpace (getNamespace dr) (
            ["", "class " ++ className dr ++ ": public docsis::common::tlv::def::TlvComplex"]
            ++ ["{", "public:"]
            ++ (dumpConstructor dr)
            ++ (boilerPlate1 (className dr))
            ++ (dumpGetters dr)
            ++ [(boilerPlate2 (className dr))]
            ++ ["", "private:"]
            ++ (map declareField $ getFields dr)
            ++ ["};"]
        ))
        ++ ["#endif"]

genBody :: DataRecord -> [String]
genBody dr =
    let includeFile = headerFileName dr
    in
        fileHeader
        ++ [incl $ intercalate "/" (getSrcPath dr ++ [includeFile])]
        ++ [""]
        ++ (doNameSpace (getNamespace dr) 
            ( (dumpRealConstructor dr) ++
              dumpGetTlvType dr ++
              dumpGetterImps (getFields dr) ++
              dumpGetNumOptionalTlvs dr ++
              dumpGetOptionalTlv dr ++
              constructFromBuffer dr
            )
            )
        
genTest :: DataRecord -> [String]
genTest dr =
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
            defineTestClass dr
        )

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

declareField :: DataField -> String
declareField fld = [i|    boost::optional<#{typeType fld}> _#{getFieldName fld};|]

indentN :: Int -> String
indentN n = concat $ replicate n "    "

indentLines :: Int -> [String] -> [String]
indentLines n l = 
    let indN = indentN n
    in map (\line -> if length line > 0 then indN ++ line else line) l

defineTestClass :: DataRecord -> [String]
defineTestClass dr =
    [
      [i|class #{className dr}Test: public Testing|]
    , "{"
    , "};"
    ]

dumpConstructor :: DataRecord -> [String]
dumpConstructor dr =  [ [i|    #{className dr}();|]
                     , ""
                     , [i|    #{className dr}(|]
                     ] ++ 
                     (indentLines 2 $ dumpFormalParameters dr) ++
                     [
                       [i|    );|]
                     ]
dumpRealConstructor :: DataRecord -> [String]
dumpRealConstructor dr =
    [
        [i|#{constructorName dr}()|]
        , "{"
        , "}"
        , ""
        , [i|#{destructorName dr}()|]
        , "{"
        , "}"
        , ""
        , [i|#{constructorName dr}(|]
    ] ++
    indentLines 1 (dumpFormalParameters dr) ++
    ["    )", "    : TlvComplex( getTlvType() )"] ++
    (braces (map (\field -> [i|buildOptional( #{tlvEnumT field}, #{getFieldName field}, _#{getFieldName field} );|]) (getFields dr) ++
        [ "updateValueLength();"]) )

constructFromBuffer :: DataRecord -> [String]
constructFromBuffer dr = [
    "",
    "/** Construct from a buffer with any length */",
    [i|#{constructorName dr}(|],
    "    const uint8_t* buff,",
    "    std::size_t availLength,",
    "    std::size_t& used )",
    "    : TlvComplex( buff, availLength )"
    ] ++
    braces ( [
        "if (getType() != getTlvType())",
        "{",
        "    throw ParseException(",
        [i|        "Unexpected message type given to #{className dr}" );|],
        "}",
        "uint16_t availLen = getLength();",
        "used = getBaseLength();",
        "while ( availLen > 0 )"
        ] ++
        braces (
            [ "std::size_t dataUsed = 0;"
            , [i|#{tlvName dr} tlv_t = static_cast<#{tlvName dr}>( *(buff + used) );|]
            ] ++
            (doSwitch "tlv_t" (map genCase $ getFields dr)
                    [
                        "discardUnknown( buff + used, availLen, dataUsed );"
                      , "break;"
                    ] ) ++
            [ "used += dataUsed;"
            , "availLen -= dataUsed;"
            ]
            ) ++
        [
            "if (availLen != 0)"
          , "{"
          , "    throw ParseException("
          , [i|       "Unused/overused length inside #{tlvName dr}" );|]
          , "}"
          , "updateValueLength();"
        ]
    )
    where
        genCase :: DataField -> (String, [String])
        genCase f = (tlvEnumT f,
                     [     [i|_#{getFieldName f} = #{typeType f}( buff + used, availLen, dataUsed );|]
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

dumpFormalParameters :: DataRecord -> [String]
dumpFormalParameters dr = let fieldUse = map (\f -> formalParameter f) (getFields dr)
                              l = length fieldUse
                          in (map (++ ",") (init fieldUse)) ++ (drop (l-1) fieldUse)

formalParameter :: DataField -> String
formalParameter df =
    [i|#{optOf $ typeVal df} #{getFieldName df}|]

dumpGetters :: DataRecord -> [String]
dumpGetters dr = map (\f -> [i|    const #{optOf $ typeType f}& get#{firstToUpper $ getFieldName f}() const;|] ) (getFields dr)

dumpGetterImps :: [DataField] -> [String]
dumpGetterImps [] = []
dumpGetterImps (f: fs) = [
                           ""
                         , [i|const #{optOf $ typeType f}& #{methodName f ("get" ++ (firstToUpper $ refName f))}() const|]
                         ] ++
                         braces [[i|return _#{getFieldName f};|]] ++
                         dumpGetterImps fs
                        
dumpGetTlvType :: DataRecord -> [String]
dumpGetTlvType r =
    [
      ""
    , [i|uint8_t #{methodName r "getTlvType"}()|]
    ] ++
    braces [[i|return static_cast<uint8_t>( #{getTlvBaseRecord r}::#{upperSnakeCase $ className r};|]]

dumpGetNumOptionalTlvs :: DataRecord -> [String]
dumpGetNumOptionalTlvs dr = [
                             ""
                           , [i|std::size_t #{methodName dr "getNumOptionalTlvs"}() const|]
                           ] ++
                           braces [[i|return #{length $ getFields dr};|]]

dumpGetOptionalTlv :: DataRecord -> [String]
dumpGetOptionalTlv dr =
    [ ""
    , [i|#{optOf "const TlvBase_t<uint16_t>&"} #{methodName dr "getOptionalTlv"}( std::size_t index ) const|]
    ] ++
    braces (
            doSwitch 
                "index" 
                (map genCaseN $ (getFields dr) `zip` [0..])
                [[i|    throw std::range_error( "Index out of range for #{tlvName dr}" );|]]
        )
    where
        genCaseN :: (DataField, Int) -> (String, [String])
        genCaseN (f, n) = (show n, [[i|return getOptBase16( _#{getFieldName f} );|]])

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