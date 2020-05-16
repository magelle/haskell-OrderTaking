module OrderTaking.Implementation.ImplementationSpec (spec) where


import           Test.Hspec (Spec, hspec, describe, it, shouldBe)
import OrderTaking.Implementation.Implementation
import OrderTaking.PublicTypes.PublicTypes
import Data.Either

spec :: Spec
spec = describe "Implementation" $ do
    describe "toCustomerInfo" $ do
        it "should return the error when Firstname is empty" $
            toCustomerInfo UnvalidatedCustomerInfo {
                uciFirstName = ""
                , uciLastName = "GELLE"
                , uciEmailAddress = "maxime.gelle@test.com"
                , uciVipStatus = "VIP"
            } `shouldBe` Left "FirstName must not be empty"
        it "should return the error when Lastname is empty" $
            toCustomerInfo UnvalidatedCustomerInfo {
                uciFirstName = "Maxime"
                , uciLastName = ""
                , uciEmailAddress = "maxime.gelle@test.com"
                , uciVipStatus = "VIP"
            } `shouldBe` Left "LastName must not be empty"
        it "should return the error when email address is empty" $
            toCustomerInfo UnvalidatedCustomerInfo {
                uciFirstName = "Maxime"
                , uciLastName = "Gelle"
                , uciEmailAddress = ""
                , uciVipStatus = "VIP"
            } `shouldBe` Left "EmailAddress must not be empty"
        it "should return the error when VIPStatus is empty" $
            toCustomerInfo UnvalidatedCustomerInfo {
                uciFirstName = "Maxime"
                , uciLastName = "GELLE"
                , uciEmailAddress = "maxime.gelle@test.com"
                , uciVipStatus = ""
            } `shouldBe` Left "vipStatus: Must be one of 'Normal', 'VIP'"
        it "should return a valid customer when everything is ok" $
            isRight (toCustomerInfo UnvalidatedCustomerInfo {
                uciFirstName = "Maxime"
                , uciLastName = "GELLE"
                , uciEmailAddress = "maxime.gelle@test.com"
                , uciVipStatus = "VIP"
            }) `shouldBe` True