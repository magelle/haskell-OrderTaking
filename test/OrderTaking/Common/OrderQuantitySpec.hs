module OrderTaking.Common.OrderQuantitySpec ( spec ) where


import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Common.OrderQuantity as OrderQuantity
import           OrderTaking.Common.ProductCode as ProductCode
import           OrderTaking.Common.Result

spec :: Spec
spec = describe "OrderQuantity" $ do
    describe "Create a OrderQuantity" $ do
        it "should accept to create unit quantity"
            $ isOk (OrderQuantity.create "myfieldName" unitProductCode 10.0) `shouldBe` True
        it "should accept to create unit quantity"
            $ valueR (OrderQuantity.create "myfieldName" unitProductCode 10.0) `shouldBe` 10.0
        it "should accept to create kilo quantity"
            $ isOk (OrderQuantity.create "myfieldName" kiloProductCode 12.5) `shouldBe` True
        it "should accept to create kilo quantity"
            $ valueR (OrderQuantity.create "myfieldName" kiloProductCode 12.5) `shouldBe` 12.5

unitProductCode :: ProductCode
unitProductCode = okR (ProductCode.create "field" "W1234")
kiloProductCode :: ProductCode
kiloProductCode = okR (ProductCode.create "field" "G1234")

okR (Ok a) = a
valueR (Ok a) = OrderQuantity.value a