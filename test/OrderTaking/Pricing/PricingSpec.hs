module OrderTaking.Pricing.PricingSpec
    ( spec
    )
where

import           Test.Hspec (Spec, hspec, describe, it, shouldBe)
import           OrderTaking.Pricing.Pricing
import           OrderTaking.Common.PromotionCode
import qualified OrderTaking.Common.ProductCode as ProductCode
import qualified OrderTaking.Common.Price as Price
import           OrderTaking.PublicTypes.PublicTypes
import           OrderTaking.InternalTypes.InternalTypes

spec :: Spec
spec = describe "Pricing" $ do
    describe "createPricingMethod" $ do
        it "return stantard when empty" $
            createPricingMethod "" `shouldBe` Standard
        it "return stantard when blank" $
            createPricingMethod "   \n\r" `shouldBe` Standard
        it "return PromotionCode otherwise" $
            createPricingMethod "123ABC" `shouldBe` Promotion (MkPromotionCode "123ABC")
    
    describe "getPricingFunction" $ do
        it "get the standard price when not promotion" $
            getPricingFunction standardPricing promotionWithProductPrice Standard productCode 
            `shouldBe` standardPrice
        it "get the promotion price when there is a promotion code" $
            getPricingFunction standardPricing promotionWithProductPrice promotionCode productCode 
            `shouldBe` priceInPromotion
        it "get the promotion price when there is a promotion code" $
            getPricingFunction standardPricing promotionWithoutProductPrice promotionCode productCode 
            `shouldBe` standardPrice

promotionCode = (Promotion (MkPromotionCode "PROMO"))
productCode = extract $ ProductCode.create "" "G1234"
standardPrice = extract $ Price.create 5.0
priceInPromotion = extract $ Price.create 5.0

standardPrices = \pc -> case pc of productCode -> standardPrice
priceWithPromotion = \pc -> case pc of productCode -> Just $ priceInPromotion
noPriceInPromotion = \pc -> case pc of productCode -> Nothing

standardPricing () = standardPrices
promotionWithProductPrice promotionCode = priceWithPromotion
promotionWithoutProductPrice promotionCode = noPriceInPromotion

extract (Right a) = a 