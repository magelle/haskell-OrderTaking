module OrderTaking.Common.EmailAddressSpec ( spec ) where


import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Common.EmailAddress
import           OrderTaking.Common.Result

spec :: Spec
spec = describe "EmailAddress" $
    describe "Create a valid email address" $ do
        it "should accept to create" $
            isOk (create "myfieldName" "user@mail.com") `shouldBe` True
        it "should create a string with the right value"
            $ valueR (create "myfieldName" "user@mail.com") `shouldBe` "user@mail.com"
        it "should refuse to create an empty string" $
            create "myfieldName" "" `shouldBe` Error "myfieldName must not be empty"
        it "should refuse to create an empty string" $
            create "myfieldName" "usermail.com" `shouldBe` Error "myfieldName : 'usermail.com' must match the pattern '^.+@.+$'"

valueR (Ok a) = value a