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

instance Show TlvType
    where show a = [i|::#{index a} #{description a}|]

optOf :: String -> String
optOf s = [i|boost::optional<#{s}>|]

data Field = Field { fieldName :: String        -- ^name of the field
                   , fieldType :: TlvType        -- ^type of the field
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
                                 , baseTlvName :: String
                                 }
                                 deriving Show               

showField, showTlvField, showValField :: Record -> TlvType -> String
showField    r t = [i|#{recordName r}#{index t}Type|]
showTlvField r t = [i|#{baseTlv r}::#{tlvEnum t}|]
showValField r t = [i|#{showField r t}::VALUE_TYPE|]

baseTlv r = [i|#{recordName r}TLV_t|]