module OrderTaking.Common.KilogramQuantitySpec ( spec ) where


import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Common.KilogramQuantity
import           OrderTaking.Common.Result

spec :: Spec
spec = describe "KilogramQuantity" $ do
    describe "Create a unit quantity" $ do
        it "should accept to create unit quantity"
            $ isOk (create "myfieldName" 50.0) `shouldBe` True
        it "should create a unit quanity with the right value"
            $ valueR (create "myfieldName" 56.0) `shouldBe` 56.0
        it "should refuse to create a unit quantity under limits"
            $ create "myfieldName" 0.0 `shouldBe` Error "myfieldName: Must not be less than 5.0e-2"
        it "should refuse to create a unit quantity under limits"
            $ create "myfieldName" 100000.1 `shouldBe` Error "myfieldName: Must not be greater than 100000.0"

valueR (Ok a) = value a