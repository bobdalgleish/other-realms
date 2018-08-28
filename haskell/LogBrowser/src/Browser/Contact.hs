module Browser.Contact where

import LogRecord.Log
import Browser.Predicate

contactOnly :: Predicate (Log String)
contactOnly = AnyOf $ [
                        Test $ moduleIs "EquipmentSelectorBase.java",
                        Test $ moduleIs "ContactSupervisor.java"
                       ]

-- |Select all logs referencing the satellite id
forSatellite :: String -> Predicate (Log String)
forSatellite satellite = AllOf [
                                 contactOnly,
                                 Test $ bodyHas $ satelliteForm satellite
                                ]
                         where
                            satelliteForm s = " " ++ (normalize s) ++ "-"
                            normalize s = reverse $ take 5 (reverse s ++ "0000")