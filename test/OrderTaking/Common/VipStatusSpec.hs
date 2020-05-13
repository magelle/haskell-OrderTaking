module OrderTaking.Common.VipStatusSpec ( spec ) where

import Data.Either
import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Common.VipStatus
import           OrderTaking.Common.Result

spec :: Spec
spec = describe "VipStatus" $ do
    describe "Create a vip status" $ do
        it "should accept to create" $ do
            isRight (create "myfieldName" "normal") `shouldBe` True
        it "should create a VipStatus normal" $ do
            valueR (create "myfieldName" "normal") `shouldBe` Normal
        it "should create a VipStatus Normal" $ do
            valueR (create "myfieldName" "Normal") `shouldBe` Normal
        it "should create a VipStatus vip" $ do
            valueR (create "myfieldName" "vip") `shouldBe` Vip
        it "should create a VipStatus VIP" $ do
            valueR (create "myfieldName" "VIP") `shouldBe` Vip
        it "should return an error otherwise" $ do 
            create "myfieldName" "text" `shouldBe` Left "myfieldName: Must be one of 'Normal', 'VIP'"

valueR (Right a) = a