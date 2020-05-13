module OrderTaking.Common.PriceSpec ( spec ) where

import Data.Either
import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Common.Price
import           OrderTaking.Common.Result

spec :: Spec
spec = describe "Price" $ do
    describe "Create a Price" $ do
        it "should accept to create unit quantity" $
            isRight (create 50.0) `shouldBe` True
        it "should create a unit quanity with the right value"$
            valueR (create 56.0) `shouldBe` 56.0
        it "should refuse to create a unit quantity under limits" $
            create (-0.1) `shouldBe` Left "Price: Must not be less than 0.0"
        it "should refuse to create a unit quantity under limits" $
            create 1000.1 `shouldBe` Left "Price: Must not be greater than 1000.0"
    describe "Multiply a Price" $ do
        it "should accept to create unit quantity" $
            let price = unwrapR (create 50.0) in
                valueR (multiply 2.5 price) `shouldBe` 125.0

valueR (Right a) = value a
unwrapR (Right a) = a