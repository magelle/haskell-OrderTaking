module OrderTaking.InternalTypes.InternalTypes (
    AddressValidationLeft(..),
CheckedAddress(..),
PricingMethod(..),
ValidatedOrderLine(..),
ValidatedOrder(..),
PricedOrderProductLine(..),
PricedOrderLine(..),
PricedOrder(..),
ShippingMethod(..),
ShippingInfo(..),
PricedOrderWithShippingMethod(..),
HtmlString(..),
OrderAcknowledgment(..),
SendResult(..),
CheckProductCodeExists,
CheckAddressExists,
ValidateOrder,
GetProductPrice,
TryGetProductPrice,
GetPricingFunction,
GetStandardPrices,
GetPromotionPrices,
PriceOrder,
CalculateShippingCost,
AddShippingInfoToOrder,
FreeVipShipping,
CreateOrderAcknowledgmentLetter,
SendOrderAcknowledgment,
AcknowledgeOrder,
CreateEvents,
) where

import Data.Maybe
import OrderTaking.Common.ProductCode
import OrderTaking.Common.PromotionCode
import OrderTaking.Common.OrderLineId
import OrderTaking.Common.OrderQuantity
import OrderTaking.Common.OrderId
import OrderTaking.Common.Price
import OrderTaking.Common.BillingAmount
import OrderTaking.Common.EmailAddress
import OrderTaking.Common.Result
import OrderTaking.CompoundTypes.CustomerInfo
import OrderTaking.CompoundTypes.Address
import OrderTaking.PublicTypes.PublicTypes

-- ======================================================
-- Define each step in the PlaceOrder workflow using internal types 
-- (not exposed outside the bounded context)
-- ======================================================

-- ---------------------------
-- Validation step
-- ---------------------------

-- Product validation

type CheckProductCodeExists = ProductCode -> Bool

-- Address validation
data AddressValidationLeft = InvalidFormat | AddressNotFound 

data CheckedAddress = CheckedAddress UnvalidatedAddress

type CheckAddressExists = 
    UnvalidatedAddress -> AsyncResult CheckedAddress AddressValidationLeft

-- ---------------------------
-- Validated Order 
-- ---------------------------

data PricingMethod = Standard | Promotion PromotionCode 

data ValidatedOrderLine = ValidatedOrderLine {
    volOrderLineId :: OrderLineId 
    , volProductCode :: ProductCode 
    , volQuantity :: OrderQuantity
    }

data ValidatedOrder = ValidatedOrder {
    voOrderId :: OrderId
    , voCustomerInfo :: CustomerInfo
    , voShippingAddress :: Address
    , voBillingAddress :: Address
    , voLines :: [ValidatedOrderLine]
    , voPricingMethod :: PricingMethod
    }

type ValidateOrder = 
    CheckProductCodeExists  -- dependency
     -> CheckAddressExists  -- dependency
     -> UnvalidatedOrder    -- input
     -> AsyncResult ValidatedOrder ValidationLeft -- output

-- ---------------------------
-- Pricing step
-- ---------------------------


type GetProductPrice = 
    ProductCode -> Price

type TryGetProductPrice = 
    ProductCode -> Maybe Price

type GetPricingFunction = PricingMethod -> GetProductPrice

type GetStandardPrices = 
    -- no input -> return standard prices
    () -> GetProductPrice

type GetPromotionPrices = 
    -- promo input -> return prices for promo, maybe
    PromotionCode -> TryGetProductPrice 




-- priced state            
data PricedOrderProductLine = PricedOrderProductLine {
    poplOrderLineId :: OrderLineId 
    , poplProductCode :: ProductCode 
    , poplQuantity :: OrderQuantity
    , poplLinePrice :: Price
    }

data PricedOrderLine = ProductLine PricedOrderProductLine | CommentLine String

data PricedOrder = PricedOrder {
    poOrderId :: OrderId
    , poCustomerInfo :: CustomerInfo
    , poShippingAddress :: Address
    , poBillingAddress :: Address
    , poAmountToBill :: BillingAmount
    , poLines :: [PricedOrderLine]
    , poPricingMethod :: PricingMethod
    }

type PriceOrder = 
    GetPricingFunction     -- dependency
     -> ValidatedOrder  -- input
     -> Either PricingLeft PricedOrder   -- output

-- ---------------------------
-- Shipping
-- ---------------------------

data ShippingMethod = 
    PostalService 
    | Fedex24 
    | Fedex48 
    | Ups48

data ShippingInfo = ShippingInfo {
    siShippingMethod :: ShippingMethod
    , siShippingCost :: Price
    }

data PricedOrderWithShippingMethod = PricedOrderWithShippingMethod {
    powsiShippingInfo :: ShippingInfo 
    , powsiPricedOrder :: PricedOrder
    }

type CalculateShippingCost = 
    PricedOrder -> Price

type AddShippingInfoToOrder = 
    CalculateShippingCost -- dependency
     -> PricedOrder       -- input
     -> PricedOrderWithShippingMethod  -- output

-- ---------------------------
-- VIP shipping
-- ---------------------------

type FreeVipShipping =
    PricedOrderWithShippingMethod -> PricedOrderWithShippingMethod

-- ---------------------------
-- Send OrderAcknowledgment 
-- ---------------------------

data HtmlString = 
    HtmlString String

data OrderAcknowledgment = OrderAcknowledgment {
    oaEmailAddress :: EmailAddress
    , oaLetter :: HtmlString 
    }

type CreateOrderAcknowledgmentLetter =
    PricedOrderWithShippingMethod -> HtmlString

-- Send the order acknowledgement to the customer
-- Note that this does NOT generate an Result-type error (at least not in this workflow)
-- because on failure we will continue anyway.
-- On success, we will generate a OrderAcknowledgmentSent event,
-- but on failure we won't.

data SendResult = Sent | NotSent

type SendOrderAcknowledgment =
    OrderAcknowledgment -> SendResult 
    
type AcknowledgeOrder = 
    CreateOrderAcknowledgmentLetter  -- dependency
     -> SendOrderAcknowledgment      -- dependency
     -> PricedOrderWithShippingMethod  -- input
     -> Maybe OrderAcknowledgmentSent -- output

-- ---------------------------
-- Create events
-- ---------------------------

type CreateEvents = 
    PricedOrder                           -- input
     -> Maybe OrderAcknowledgmentSent    -- input (event from previous step)
     -> [PlaceOrderEvent]              -- output

