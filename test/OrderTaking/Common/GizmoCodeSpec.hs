module OrderTaking.Common.GizmoCodeSpec ( spec ) where

import Data.Either
import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Common.GizmoCode
import           OrderTaking.Common.Result

spec :: Spec
spec = describe "GizmoCode" $
    describe "Create a vip status" $ do
        it "should accept to create"
            $ isRight (create "myfieldName" "G1234") `shouldBe` True
        it "should create a GizmoCode with value"
            $ valueR (create "myfieldName" "G1234") `shouldBe` "G1234"
        it "should refuse too small GizmoCode"
            $ create "myfieldName" "G123" `shouldBe` Left "myfieldName : 'G123' must match the pattern '^G[0-9]{4}$'"
        it "should refuse too long GizmoCode"
            $ create "myfieldName" "G12345" `shouldBe` Left "myfieldName : 'G12345' must match the pattern '^G[0-9]{4}$'"
        it "should refuse GizmoCode with non digit"
            $ create "myfieldName" "Gabcd" `shouldBe` Left "myfieldName : 'Gabcd' must match the pattern '^G[0-9]{4}$'"
        it "should refuse GizmoCode without the W"
            $ create "myfieldName" "1234" `shouldBe` Left "myfieldName : '1234' must match the pattern '^G[0-9]{4}$'"

valueR (Right a) = value a