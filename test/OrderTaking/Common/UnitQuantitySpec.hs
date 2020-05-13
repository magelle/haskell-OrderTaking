module OrderTaking.Common.UnitQuantitySpec ( spec ) where

import Data.Either
import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Common.UnitQuantity
import           OrderTaking.Common.Result

spec :: Spec
spec = describe "UnitQuantity" $ do
    describe "Create a unit quantity" $ do
        it "should accept to create unit quantity"
            $ isRight (create "myfieldName" 50) `shouldBe` True
        it "should create a unit quanity with the right value"
            $ valueR (create "myfieldName" 56) `shouldBe` 56
        it "should refuse to create a unit quantity under limits"
            $ create "myfieldName" 0 `shouldBe` Left "myfieldName: Must not be less than 1"
        it "should refuse to create a unit quantity under limits"
            $ create "myfieldName" 1001 `shouldBe` Left "myfieldName: Must not be greater than 1000"

valueR (Right a) = value a