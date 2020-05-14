module OrderTaking.Implementation.ImplementationSpec (spec) where


import           Test.Hspec (Spec, hspec, describe, it, shouldBe)
import OrderTaking.Implementation.Implementation
import OrderTaking.PublicTypes.PublicTypes

spec :: Spec
spec = describe "Implementation" $ do
    describe "toCustomerInfo" $ do
        it "should agregate errors" $
            toCustomerInfo UnvalidatedCustomerInfo {
                uciFirstName = ""
                , uciLastName = ""
                , uciEmailAddress = ""
                , uciVipStatus = "VIP"
            } `shouldBe` Left "FirstName must not be empty"