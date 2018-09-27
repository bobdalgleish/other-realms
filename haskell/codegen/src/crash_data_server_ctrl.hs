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
                      , usingNamespaces = []
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
                 , classIncludes = []
                 }

main = do
        codegen $ recordOf family record2
