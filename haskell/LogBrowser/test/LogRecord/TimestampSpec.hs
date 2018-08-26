module LogRecord.TimestampSpec (spec) where

import Test.Hspec
import LogRecord.Timestamp

spec :: Spec
spec = do
  describe "Timestamp fromYear" $ do
    describe "fromYear for values over 70" $ do
      it "returns 1971 when given 71" $
        fromYear 71 `shouldBe` 1971

      it "returns 1999 when given 99" $
        fromYear 99 `shouldBe` 1999
        fromYear 1999 `shouldBe` 1999

    describe "fromYear for values under 70" $ do

      it "returns 2000 when given 0" $
        fromYear 0 `shouldBe` 2000

      it "returns 2001 when given 1" $
        fromYear 1 `shouldBe` 2001
  
