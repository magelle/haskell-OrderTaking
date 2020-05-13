module OrderTaking.Pricing.Pricing
    ( createPricingMethod
    , getPricingFunction
    )
where

-- Move all procing logic into its own module,
-- as it will likely get complicated!

import           OrderTaking.Util
import           OrderTaking.Common.PromotionCode
import           OrderTaking.Common.ProductCode
import           OrderTaking.Common.Price
import           OrderTaking.PublicTypes.PublicTypes
import           OrderTaking.InternalTypes.InternalTypes


-- An internal helper module to help with pricing


-- Create a pricing method given a promotionCode on the unvalidated order form
-- If null -> Standard otherwise wrap in PromotionCode
createPricingMethod :: String -> PricingMethod
createPricingMethod promotionCode
    | isBlank promotionCode = Standard
    | otherwise             = Promotion (MkPromotionCode promotionCode)

getPricingFunction
    :: GetStandardPrices -> GetPromotionPrices -> GetPricingFunction
-- the original pricing function
getPricingFunction standardPrices promotionPrices Standard = standardPrices ()
-- the promotional pricing function
getPricingFunction standardPrices promotionPrices (Promotion promotionCode) =
    let tryGetProductPrice = promotionPrices promotionCode
    in  \productCode -> case (tryGetProductPrice productCode) of
            Just price -> price
            Nothing    -> standardPrices () productCode
