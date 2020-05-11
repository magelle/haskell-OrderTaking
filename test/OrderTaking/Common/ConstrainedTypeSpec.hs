module OrderTaking.Common.ConstrainedTypeSpec ( spec ) where


import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Common.ConstrainedType
import           OrderTaking.Common.Result

spec :: Spec
spec = describe "Contrained Types contructors" $ do
    describe "Create a constrained string" $ do
        it "should accept to create" $ do 
            createString "myfieldName" id 10 "value" `shouldBe` Ok "value"
        it "should refuse to create an empty string" $ do 
            createString "myfieldName" id 10 "" `shouldBe` Error "myfieldName must not be empty"
        it "should refuse to create a too long string" $ do 
            createString "myfieldName" id 9 "0123456789" `shouldBe` Error "myfieldName must not be more than 9 chars"

    describe "Create a constrained string, maybe" $ do
        it "should accept to create" $ do
            createStringOption "myfieldName" id 10 "value" `shouldBe` Ok (Just "value")
        it "should return an empty string when empty" $ do
            createStringOption "myfieldName" id 10 "" `shouldBe` Ok Nothing
        it "should refuse to create a too long string" $ do
            createStringOption "myfieldName" id 9 "0123456789" `shouldBe` Error "myfieldName must not be more than 9 chars"

    describe "Create a constrained string that match a pattern" $ do
        it "should accept to create" $ do
            createLike "myfieldName" id ".+@.+" "user@mail.com" `shouldBe` Ok "user@mail.com"
        it "should refuse to create an empty string" $ do 
            createLike "myfieldName" id ".+@.+" "" `shouldBe` Error "myfieldName must not be empty"
        it "should refuse to create an empty string" $ do 
            createLike "myfieldName" id ".+@.+" "usermail.com" `shouldBe` Error "myfieldName : 'usermail.com' must match the pattern '.+@.+'"
