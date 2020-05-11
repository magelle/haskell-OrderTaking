module OrderTaking.Common.OrderLineIdSpec ( spec ) where


import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Common.OrderLineId
import           OrderTaking.Common.Result

spec :: Spec
spec = describe "OrderLineId" $ do
    describe "Create a OrderLineId" $ do
        it "should accept to create normal OrderLineId"
            $ isOk (create "myfieldName" "value") `shouldBe` True
        it "should create a OrderLineId with the right value"
            $ valueR (create "myfieldName" "value") `shouldBe` "value"
        it "should accept to create"
            $ isOk (create "myfieldName" "value") `shouldBe` True
        it "should refuse to create an empty OrderLineId"
            $ create "myfieldName" "" `shouldBe` Error "myfieldName must not be empty"
        it "should refuse to create a too long OrderLineId"
            $ create "myfieldName" "012345678901234567890123456789012345678901234567891" `shouldBe` Error "myfieldName must not be more than 50 chars"
    
valueR (Ok a) = value a