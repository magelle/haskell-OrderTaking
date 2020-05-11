module OrderTaking.Common.RegexSpec (spec) where

import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import OrderTaking.Common.Regex

spec :: Spec
spec = describe "Regex" $ do
    describe "matchRegex" $ do
        it "say when the string match the pattern"
            $ matchRegex ".+" "aaa" `shouldBe` True
        it "say when the string does not match the pattern"
            $ matchRegex ".+.+" "d" `shouldBe` False