module OrderTaking.Common.ZipCodeSpec ( spec ) where


import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Common.ZipCode
import           OrderTaking.Common.Result

spec :: Spec
spec = describe "ZipCode" $
    describe "Create a vip status" $ do
        it "should accept to create"
            $ isOk (create "myfieldName" "12345") `shouldBe` True
        it "should create a zipcode with value"
            $ valueR (create "myfieldName" "12345") `shouldBe` "12345"
        it "should refuse too small zipCode"
            $ create "myfieldName" "1234" `shouldBe` Error "myfieldName : '1234' must match the pattern '^[0-9]{5}$'"
        it "should refuse too long zipCode"
            $ create "myfieldName" "123456" `shouldBe` Error "myfieldName : '123456' must match the pattern '^[0-9]{5}$'"
        it "should refuse zipCode with non digit"
            $ create "myfieldName" "abcde" `shouldBe` Error "myfieldName : 'abcde' must match the pattern '^[0-9]{5}$'"

valueR (Ok a) = value a