module OrderTaking.Common.String50Spec ( spec ) where

import Data.Either
import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Common.String50
import           OrderTaking.Common.Result

spec :: Spec
spec = describe "String50" $ do
    describe "Create a String50" $ do
        it "should accept to create normal string"
            $ isRight (create "myfieldName" "value") `shouldBe` True
        it "should create a string with the right value"
            $ valueR (create "myfieldName" "value") `shouldBe` "value"
        it "should accept to create"
            $ isRight (create "myfieldName" "value") `shouldBe` True
        it "should refuse to create an empty string"
            $ create "myfieldName" "" `shouldBe` Left "myfieldName must not be empty"
        it "should refuse to create a too long string"
            $ create "myfieldName" "012345678901234567890123456789012345678901234567891" `shouldBe` Left "myfieldName must not be more than 50 chars"
    
    describe "Create aN option String50" $ do
        it "should accept to create normal string"
            $ isRight (createOption "myfieldName" "value") `shouldBe` True
        it "should create a string with the right value"
            $ valueMR (createOption "myfieldName" "value") `shouldBe` "value"
        it "should accept to create"
            $ isRight (createOption "myfieldName" "value") `shouldBe` True
        it "should refuse to create an empty string"
            $ createOption "myfieldName" "" `shouldBe` Right Nothing
        it "should refuse to create a too long string"
            $ createOption "myfieldName" "012345678901234567890123456789012345678901234567891" `shouldBe` Left "myfieldName must not be more than 50 chars"

valueR (Right a) = value a
valueMR (Right (Just a)) = value a