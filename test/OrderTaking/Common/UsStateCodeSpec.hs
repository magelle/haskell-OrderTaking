module OrderTaking.Common.UsStateCodeSpec ( spec ) where

import Data.Either
import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Common.UsStateCode
import           OrderTaking.Common.Result

spec :: Spec
spec = describe "UsStateCode" $
    describe "Create a UsStateCode" $ do
        it "should accept to create"
            $ isRight (create "myfieldName" "FL") `shouldBe` True
        it "should create a US state code with value"
            $ valueR (create "myfieldName" "FL") `shouldBe` "FL"
        it "should refuse bad code"
            $ create "myfieldName" "AAA" `shouldBe` Left "myfieldName : 'AAA' must match the pattern '^(A[KLRZ]|C[AOT]|D[CE]|FL|GA|HI|I[ADLN]|K[SY]|LA|M[ADEINOST]|N[CDEHJMVY]|O[HKR]|P[AR]|RI|S[CD]|T[NX]|UT|V[AIT]|W[AIVY])$'"

valueR (Right a) = value a