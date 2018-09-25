#!/usr/bin/env stack

import Record.Data
import Record.Generate
import System.IO


family :: RecordFamily
family = RecordFamily { recordFamilyName = "family"
                      , sharedIncludes = [
                            "docsis/common/tlv/def/tlv_complex.h"
                          , "gcp/message/rcp/def/rcp_field_types.h"
                          ]
                      , baseTlvName = "RpdCtrl"
                      }

record2 = Record {
                   recordName = "CrashDataServerCtrl"
                 , fields = [
                              Field "destIpAddress" 
                                (TlvType "IpAddress" "IP_ADDRESS" "40.4.1")
                            , Field "destPath" 
                                (TlvType "Path" "PATH" "40.4.2")
                            , Field "protocol" 
                                (TlvType "Protocol" "PROTOCOL" "40.4.3")
                            ]
                 , namespace = ["sedsystems", "gcp", "message", "rcp", "def"]
                 , includes = []
                 }

main = do
        writeFile (headerFileName record2) (unlines $ genHeader family record2)
        writeFile (classFileName record2) (unlines $ genBody family record2)
        writeFile (testFileName record2) (unlines $ genTest family record2)
