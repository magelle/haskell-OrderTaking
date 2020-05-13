module OrderTaking.Common.WidgetCodeSpec ( spec ) where

import Data.Either
import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Common.WidgetCode
import           OrderTaking.Common.Result

spec :: Spec
spec = describe "WidgetCode" $
    describe "Create a vip status" $ do
        it "should accept to create"
            $ isRight (create "myfieldName" "W1234") `shouldBe` True
        it "should create a WidgetCode with value"
            $ valueR (create "myfieldName" "W1234") `shouldBe` "W1234"
        it "should refuse too small WidgetCode"
            $ create "myfieldName" "W123" `shouldBe` Left "myfieldName : 'W123' must match the pattern '^W[0-9]{4}$'"
        it "should refuse too long WidgetCode"
            $ create "myfieldName" "W12345" `shouldBe` Left "myfieldName : 'W12345' must match the pattern '^W[0-9]{4}$'"
        it "should refuse WidgetCode with non digit"
            $ create "myfieldName" "Wabcd" `shouldBe` Left "myfieldName : 'Wabcd' must match the pattern '^W[0-9]{4}$'"
        it "should refuse WidgetCode without the W"
            $ create "myfieldName" "1234" `shouldBe` Left "myfieldName : '1234' must match the pattern '^W[0-9]{4}$'"

valueR (Right a) = value a