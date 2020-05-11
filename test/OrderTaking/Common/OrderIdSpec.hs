module OrderTaking.Common.OrderIdSpec ( spec ) where


import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Common.OrderId
import           OrderTaking.Common.Result

spec :: Spec
spec = describe "OrderId" $ do
    describe "Create a OrderId" $ do
        it "should accept to create normal orderId"
            $ isOk (create "myfieldName" "value") `shouldBe` True
        it "should create a orderId with the right value"
            $ valueR (create "myfieldName" "value") `shouldBe` "value"
        it "should accept to create"
            $ isOk (create "myfieldName" "value") `shouldBe` True
        it "should refuse to create an empty orderId"
            $ create "myfieldName" "" `shouldBe` Error "myfieldName must not be empty"
        it "should refuse to create a too long orderId"
            $ create "myfieldName" "012345678901234567890123456789012345678901234567891" `shouldBe` Error "myfieldName must not be more than 50 chars"
    
valueR (Ok a) = value a