module OrderTaking.Common.ProductCodeSpec ( spec ) where

import Data.Either
import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Common.ProductCode
import           OrderTaking.Common.Result

spec :: Spec
spec = describe "ProductCode" $ do
    describe "Create a product code" $
        it "should refuse to create an empty code"
            $ create "myfieldName" "" `shouldBe` Left "myfieldName: Must not be null or empty"
    describe "Create a GizmoCode" $ do
        it "should accept to create a GizmoCode"
            $ isRight (create "myfieldName" "G1234") `shouldBe` True
        it "should accept to create a GizmoCode with right value"
            $ valueR (create "myfieldName" "G1234") `shouldBe` "G1234"
        it "should refuse to create a bad GizmoCode"
            $ create "myfieldName" "G123" `shouldBe` Left "myfieldName : 'G123' must match the pattern '^G[0-9]{4}$'"
    describe "Create a WidgetCode" $ do
        it "should accept to create a WidgetCode"
            $ isRight (create "myfieldName" "W1234") `shouldBe` True
        it "should accept to create a WidgetCode with right value"
            $ valueR (create "myfieldName" "W1234") `shouldBe` "W1234"
        it "should refuse to create a bad WidgetCode"
            $ create "myfieldName" "W123" `shouldBe` Left "myfieldName : 'W123' must match the pattern '^W[0-9]{4}$'"
    describe "Create a product code" $
        it "should refuse to create an empty code"
            $ create "myfieldName" "ABC" `shouldBe` Left "myfieldName: Format not recognized 'ABC'"
        

valueR (Right a) = value a