module OrderTaking.Common.ConstrainedTypeSpec ( spec ) where


import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Common.ConstrainedType
import           OrderTaking.Common.Result

spec :: Spec
spec = describe "Contrained Types contructors" $ do
    describe "Create a constrained string" $ do
        it "should accept to create" $
            createString "myfieldName" id 10 "value" `shouldBe` Right "value"
        it "should refuse to create an empty string" $
            createString "myfieldName" id 10 "" `shouldBe` Left "myfieldName must not be empty"
        it "should refuse to create a too long string" $
            createString "myfieldName" id 9 "0123456789" `shouldBe` Left "myfieldName must not be more than 9 chars"

    describe "Create a constrained string, maybe" $ do
        it "should accept to create" $
            createStringOption "myfieldName" id 10 "value" `shouldBe` Right (Just "value")
        it "should return an empty string when empty" $
            createStringOption "myfieldName" id 10 "" `shouldBe` Right Nothing
        it "should refuse to create a too long string" $
            createStringOption "myfieldName" id 9 "0123456789" `shouldBe` Left "myfieldName must not be more than 9 chars"

    describe "Create a constrained string that match a pattern" $ do
        it "should accept to create" $
            createLike "myfieldName" id ".+@.+" "user@mail.com" `shouldBe` Right "user@mail.com"
        it "should refuse to create an empty string" $
            createLike "myfieldName" id ".+@.+" "" `shouldBe` Left "myfieldName must not be empty"
        it "should refuse to create an empty string" $
            createLike "myfieldName" id ".+@.+" "usermail.com" `shouldBe` Left "myfieldName : 'usermail.com' must match the pattern '.+@.+'"

    describe "Create a constrained Int" $ do
        it "should accept to create" $
            createInt "myfieldName" id 1 10 5 `shouldBe` Right 5
        it "should refuse to create value low than bound" $
            createInt "myfieldName" id 1 10 0 `shouldBe` Left "myfieldName: Must not be less than 1"
        it "should refuse to create value over than bound" $
            createInt "myfieldName" id 1 10 11 `shouldBe` Left "myfieldName: Must not be greater than 10"

    describe "Create a constrained Decimal" $ do
        it "should accept to create" $
            createDecimal "myfieldName" id 1.0 10.0 5.0 `shouldBe` Right 5.0
        it "should refuse to create value low than bound" $
            createDecimal "myfieldName" id 1.0 10.0 0.0 `shouldBe` Left "myfieldName: Must not be less than 1.0"
        it "should refuse to create value over than bound" $
            createDecimal "myfieldName" id 1.0 10.0 11.0 `shouldBe` Left "myfieldName: Must not be greater than 10.0"