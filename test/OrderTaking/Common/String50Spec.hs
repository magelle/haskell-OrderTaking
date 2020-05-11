module OrderTaking.Common.String50Spec ( spec ) where


import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Common.String50
import           OrderTaking.Common.Result

spec :: Spec
spec =
    describe "Create a String50" $ do
        it "should accept to create normal string"
            $ isOk (create "myfieldName" "value") `shouldBe` True
        it "should create a string with the right value"
            $ hasValue "value" (create "myfieldName" "value") `shouldBe` True
        it "should accept to create"
            $ isOk (create "myfieldName" "value") `shouldBe` True
        it "should refuse to create an empty string"
            $ create "myfieldName" "" `shouldBe` Error "myfieldName must not be empty"
        it "should refuse to create a too long string"
            $ create "myfieldName" "012345678901234567890123456789012345678901234567891" `shouldBe` Error "myfieldName must not be more than 50 chars"

hasValue expected actual = isR (valueEqual expected) actual
valueEqual expected = (isEqual expected) . value
isEqual a b = b == a