{-# LANGUAGE QuasiQuotes #-}
module Record.Data where

import Data.List
import Data.Char
import Data.String.Interpolate

data TlvType = TlvType {
                          index :: String       -- |^Index of the type
                       , tlvEnum :: String     -- |^TLV enumeration
                       , description :: String -- |^cross-reference
                       }
                       deriving Eq

instance Show TlvType
    where show a = [i|::#{index a} #{description a}|]

optOf :: String -> String
optOf s = [i|boost::optional<#{s}>|]

type IncludePath = String

data Field = Field { 
                     fieldName :: String        -- ^name of the field
                   , fieldType :: TlvType        -- ^type of the field
                   }
                   deriving (Eq, Show)

-- |Describe a data structure
data Record = Record { 
                       recordName :: String
                     , fields :: [Field]
                     , namespace :: [String]
                     , classIncludes :: [IncludePath]
                     }
                     deriving (Eq, Show)

class ClassDescription a where
  className :: a -> String
  tlvName :: a -> String
  includes :: a -> [IncludePath]

-- |Break a class name using camelCase into individual words
fileNameFragments :: String -> [String]
fileNameFragments [] = []
fileNameFragments s = let (pre, post) = break isUpper s
                in case pre of
                    [] -> let r = tail post
                              (rf, rb) = break isUpper r
                        in (head post:rf) : fileNameFragments rb
                    word -> word : fileNameFragments post

data RecordFamily = RecordFamily { recordFamilyName :: String
                                 , sharedIncludes :: [IncludePath]
                                 , baseTlvName :: String
                                 , usingNamespaces :: [[String]]
                                 }
                                 deriving (Eq, Show)

data DataRecord = DataRecord {
                               dataRecordFamily :: RecordFamily
                             , dataRecord :: Record
                             }
                             deriving (Eq, Show)

recordOf :: RecordFamily -> Record -> DataRecord
recordOf f r = DataRecord f r

instance ClassDescription DataRecord where
  className dr = recordName $ dataRecord dr
  tlvName dr   = [i|#{className dr}TLV_t|]
  includes dr  = (sharedIncludes $ dataRecordFamily dr) ++ (classIncludes $ dataRecord dr)

getTlvBaseRecord, baseFileName, headerFileName, classFileName, testFileName :: DataRecord -> String
getTlvBaseRecord dr = (baseTlvName $ dataRecordFamily dr) ++ "TLV_t"
baseFileName dr     = map toLower $ intercalate "_" $ fileNameFragments $ recordName $ dataRecord dr
headerFileName dr   = baseFileName dr ++ ".h"
classFileName dr    = baseFileName dr ++ ".cpp"
testFileName dr     = baseFileName dr ++ "_test.cpp"

getNamespace :: DataRecord -> [String]
getNamespace dr = namespace $ dataRecord dr

getSrcPath :: DataRecord -> [String]
getSrcPath dr = tail $ getNamespace dr

data DataField = DataField {  
                             dataFieldFamily :: RecordFamily
                           , dataFieldRecord :: Record
                           , field :: Field
                           }
                           deriving (Eq, Show)

instance ClassDescription DataField where
  className df = recordName $ dataFieldRecord df
  tlvName   df = [i|#{className df}TLV_t|]
  includes  df = (sharedIncludes $ dataFieldFamily df) ++ (classIncludes $ dataFieldRecord df)


fieldOf :: DataRecord -> Field -> DataField
fieldOf dr f = DataField (dataRecordFamily dr) (dataRecord dr) f

getFields :: DataRecord -> [DataField]
getFields dr = map (\f -> fieldOf dr f) $ fields $ dataRecord dr

getClassOf :: DataField -> String
getClassOf df = recordName $ dataFieldRecord df

showF, showBaseTlv, showTlv, showValType :: DataField -> String
showF df       = [i|#{recordName $ dataFieldRecord df}#{index $ getFieldType df}Type|]
showBaseTlv df = [i|#{recordName $ dataFieldRecord df}TLV_t|]
showTlv df     = [i|#{showBaseTlv df}::#{tlvEnum $ getFieldType df}|]
showValType df = [i|#{showF df}::VALUE_TYPE|]

getFieldName :: DataField -> String
getFieldName df = fieldName $ field df

getFieldType :: DataField -> TlvType
getFieldType df = fieldType $ field df
