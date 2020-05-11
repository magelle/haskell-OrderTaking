module OrderTaking.Common.ConstrainedTypeSpec ( spec ) where


import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Common.ConstrainedType
import           OrderTaking.Common.Result

spec :: Spec
spec =
    describe "Create a constrained string" $ do
        it "should accept to create"
            $ createString "myfieldName" id 10 "value" `shouldBe` Ok "value"
        it "should refuse to create an empty string"
            $ createString "myfieldName" id 10 "" `shouldBe` Error "myfieldName must not be empty"
        it "should refuse to create a too long string"
            $ createString "myfieldName" id 9 "0123456789" `shouldBe` Error "myfieldName must not be more than 9 chars"
