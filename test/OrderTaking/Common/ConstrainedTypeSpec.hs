module OrderTaking.Common.ConstrainedTypeSpec ( spec ) where


import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Common.ConstrainedType
import           OrderTaking.Common.Result

spec :: Spec
spec = describe "Contrained Types contructors" $ do
    describe "Create a constrained string" $ do
        it "should accept to create" $
            createString "myfieldName" id 10 "value" `shouldBe` Ok "value"
        it "should refuse to create an empty string" $
            createString "myfieldName" id 10 "" `shouldBe` Error "myfieldName must not be empty"
        it "should refuse to create a too long string" $
            createString "myfieldName" id 9 "0123456789" `shouldBe` Error "myfieldName must not be more than 9 chars"

    describe "Create a constrained string, maybe" $ do
        it "should accept to create" $
            createStringOption "myfieldName" id 10 "value" `shouldBe` Ok (Just "value")
        it "should return an empty string when empty" $
            createStringOption "myfieldName" id 10 "" `shouldBe` Ok Nothing
        it "should refuse to create a too long string" $
            createStringOption "myfieldName" id 9 "0123456789" `shouldBe` Error "myfieldName must not be more than 9 chars"

    describe "Create a constrained string that match a pattern" $ do
        it "should accept to create" $
            createLike "myfieldName" id ".+@.+" "user@mail.com" `shouldBe` Ok "user@mail.com"
        it "should refuse to create an empty string" $
            createLike "myfieldName" id ".+@.+" "" `shouldBe` Error "myfieldName must not be empty"
        it "should refuse to create an empty string" $
            createLike "myfieldName" id ".+@.+" "usermail.com" `shouldBe` Error "myfieldName : 'usermail.com' must match the pattern '.+@.+'"

    describe "Create a constrained Int" $ do
        it "should accept to create" $
            createInt "myfieldName" id 1 10 5 `shouldBe` Ok 5
        it "should refuse to create value low than bound" $
            createInt "myfieldName" id 1 10 0 `shouldBe` Error "myfieldName: Must not be less than 1"
        it "should refuse to create value over than bound" $
            createInt "myfieldName" id 1 10 11 `shouldBe` Error "myfieldName: Must not be greater than 10"