module OrderTaking.Common.ProductCodeSpec ( spec ) where


import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Common.ProductCode
import           OrderTaking.Common.Result

spec :: Spec
spec = describe "ProductCode" $ do
    describe "Create a product code" $
        it "should refuse to create an empty code"
            $ create "myfieldName" "" `shouldBe` Error "myfieldName: Must not be null or empty"
    describe "Create a GizmoCode" $ do
        it "should accept to create a GizmoCode"
            $ isOk (create "myfieldName" "G1234") `shouldBe` True
        it "should accept to create a GizmoCode with right value"
            $ valueR (create "myfieldName" "G1234") `shouldBe` "G1234"
        it "should refuse to create a bad GizmoCode"
            $ create "myfieldName" "G123" `shouldBe` Error "myfieldName : 'G123' must match the pattern '^G[0-9]{4}$'"
    describe "Create a WidgetCode" $ do
        it "should accept to create a WidgetCode"
            $ isOk (create "myfieldName" "W1234") `shouldBe` True
        it "should accept to create a WidgetCode with right value"
            $ valueR (create "myfieldName" "W1234") `shouldBe` "W1234"
        it "should refuse to create a bad WidgetCode"
            $ create "myfieldName" "W123" `shouldBe` Error "myfieldName : 'W123' must match the pattern '^W[0-9]{4}$'"
    describe "Create a product code" $
        it "should refuse to create an empty code"
            $ create "myfieldName" "ABC" `shouldBe` Error "myfieldName: Format not recognized 'ABC'"
        

valueR (Ok a) = value a