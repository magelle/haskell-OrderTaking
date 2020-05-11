module OrderTaking.Common.RegexSpec (spec) where

import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import OrderTaking.Common.Regex

spec :: Spec
spec = describe "Regex" $
    describe "matchRegex" $ do
        it "say when the string match the pattern"
            $ matchRegex ".+" "aaa" `shouldBe` True
        it "say when the string does not match the pattern"
            $ matchRegex ".+.+" "d" `shouldBe` False
        it "say when the string does not match the pattern"
            $ matchRegex "^[0-9]{5}$" "123456" `shouldBe` False