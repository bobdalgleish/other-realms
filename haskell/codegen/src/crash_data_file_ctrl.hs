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
       
record1 = Record { recordName = "CrashDataFileCtrl"
                 , fields = [
                              Field "index"
                                (TlvType "Index" "INDEX" "40.3.1")
                            , Field "fileControl" 
                                (TlvType "FileControl" "FILE_CONTROL" "40.3.2")
                            ]
                 , namespace = ["sedsystems", "gcp", "message", "rcp", "def"]
                 , classIncludes = [
                              ]
                 }


main = do
        codegen $ recordOf family record1
            