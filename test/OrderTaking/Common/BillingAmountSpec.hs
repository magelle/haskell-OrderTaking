module OrderTaking.Common.BillingAmountSpec ( spec ) where


import           Data.Either
import           Data.List as List
import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Common.BillingAmount as BillingAmount
import           OrderTaking.Common.Result
import           OrderTaking.Common.Price as Price

spec :: Spec
spec = describe "BillingAmount" $ do
    describe "Create a billing amount" $ do
        it "should accept to create billing amount" $
            isRight (BillingAmount.create 50.0) `shouldBe` True
        it "should create a billing amount with the right value" $
            valueR (BillingAmount.create 56.0) `shouldBe` 56.0
        it "should refuse to create a billing amount under limits" $
            BillingAmount.create (-0.1) `shouldBe` Left "BillingAmount: Must not be less than 0.0"
        it "should refuse to create a billing amount under limits" $
            BillingAmount.create 10000.1 `shouldBe` Left "BillingAmount: Must not be greater than 10000.0"
    describe "Sum prices" $ do
        it "should Sum prices to create a billing amount" $
            (valueR . BillingAmount.sumPrices) (priceList [10.5, 9.5]) `shouldBe` 20

valueR (Right a) = BillingAmount.value a
unwrapR (Right a) = a
priceList = (List.map unwrapR) . (List.map Price.create)