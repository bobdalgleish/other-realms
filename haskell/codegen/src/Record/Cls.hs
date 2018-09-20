module Record.Cls where

import Record.Data
import System.IO

family :: RecordFamily
family = RecordFamily { recordFamilyName = "family"
                      , sharedIncludes = [
                            "docsis/common/tlv/def/tlv_complex.h"
                          , "gcp/message/rcp/def/rcp_field_types.h"
                          ]
                      }
       
record1 = Record { recordName = "CrashDataFileCtrl"
                 , fields = [
                              Field "index"
                                (TlvType "CrashDataFileControl" "Index" "INDEX")
                                "40.3.1"
                            , Field "fileControl" 
                                (TlvType "CrashDataFileControl" "FileControl" "FILE_CONTROL")
                                "40.3.2"
                            ]
                 , namespace = ["sedsystems", "gcp", "message", "rcp", "def"]
                 , includes = [
                              ]
                 }

record2 = Record {
                   recordName = "CrashDataServerCtrl"
                 , fields = [
                              Field "destIpAddress" 
                                (TlvType "CrashDataServer" "IpAddress" "IP_ADDRESS")
                                "40.4.1"
                            , Field "destPath" 
                                (TlvType "CrashDataServer" "path" "PATH")
                                "40.4.2"
                            , Field "protocol" 
                                (TlvType "CrashDataServer" "Protocol" "PROTOCOL")
                                "40.4.3"
                            ]
                 , namespace = ["sedsystems", "gcp", "message", "rcp", "def"]
                 , includes = []
                 }

main = do
        writeFile (headerFileName record1) (unlines $ genHeader family record1)
        writeFile (classFileName record1) (unlines $ genBody family record1)