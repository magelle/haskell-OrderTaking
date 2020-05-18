module OrderTaking.InternalTypes.InternalTypes
    ( AddressValidationLeft(..)
    , CheckedAddress(..)
    , PricingMethod(..)
    , ValidatedOrderLine(..)
    , ValidatedOrder(..)
    , PricedOrderProductLine(..)
    , PricedOrderLine(..)
    , PricedOrder(..)
    , ShippingMethod(..)
    , ShippingInfo(..)
    , PricedOrderWithShippingMethod(..)
    , HtmlString(..)
    , OrderAcknowledgment(..)
    , SendResult(..)
    , CheckProductCodeExists
    , CheckAddressExists
    , ValidateOrder
    , GetProductPrice
    , TryGetProductPrice
    , GetPricingFunction
    , GetStandardPrices
    , GetPromotionPrices
    , PriceOrder
    , CalculateShippingCost
    , AddShippingInfoToOrder
    , FreeVipShipping
    , CreateOrderAcknowledgmentLetter
    , SendOrderAcknowledgment
    , AcknowledgeOrder
    , CreateEvents
    )
where

import           Data.Maybe
import           OrderTaking.Common.ProductCode
import           OrderTaking.Common.PromotionCode
import           OrderTaking.Common.OrderLineId
import           OrderTaking.Common.OrderQuantity
import           OrderTaking.Common.OrderId
import           OrderTaking.Common.Price
import           OrderTaking.Common.BillingAmount
import           OrderTaking.Common.EmailAddress
import           OrderTaking.Common.Result
import           OrderTaking.CompoundTypes.CustomerInfo
import           OrderTaking.CompoundTypes.Address
import           OrderTaking.PublicTypes.PublicTypes

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
data AddressValidationLeft = InvalidFormat | AddressNotFound deriving (Eq, Show)

data CheckedAddress = CheckedAddress UnvalidatedAddress deriving (Eq, Show)

type CheckAddressExists
    = UnvalidatedAddress -> IOResult AddressValidationLeft CheckedAddress 

-- ---------------------------
-- Validated Order 
-- ---------------------------

data PricingMethod = Standard | Promotion PromotionCode deriving (Eq, Show)

data ValidatedOrderLine = ValidatedOrderLine {
    volOrderLineId :: OrderLineId
    , volProductCode :: ProductCode
    , volQuantity :: OrderQuantity
    } deriving (Eq, Show)

data ValidatedOrder = ValidatedOrder {
    voOrderId :: OrderId
    , voCustomerInfo :: CustomerInfo
    , voShippingAddress :: Address
    , voBillingAddress :: Address
    , voLines :: [ValidatedOrderLine]
    , voPricingMethod :: PricingMethod
    } deriving (Eq, Show)

type ValidateOrder
    =  CheckProductCodeExists  -- dependency
    -> CheckAddressExists  -- dependency
    -> UnvalidatedOrder    -- input
    -> IOResult ValidationLeft ValidatedOrder -- output

-- ---------------------------
-- Pricing step
-- ---------------------------


type GetProductPrice = ProductCode -> Price

type TryGetProductPrice = ProductCode -> Maybe Price

type GetPricingFunction = PricingMethod -> GetProductPrice

type GetStandardPrices
    =
    -- no input -> return standard prices
      () -> GetProductPrice

type GetPromotionPrices
    =
    -- promo input -> return prices for promo, maybe
      PromotionCode -> TryGetProductPrice




-- priced state            
data PricedOrderProductLine = PricedOrderProductLine {
    poplOrderLineId :: OrderLineId
    , poplProductCode :: ProductCode
    , poplQuantity :: OrderQuantity
    , poplLinePrice :: Price
    } deriving (Eq, Show)

data PricedOrderLine = ProductLine PricedOrderProductLine | CommentLine String deriving (Eq, Show)

data PricedOrder = PricedOrder {
    poOrderId :: OrderId
    , poCustomerInfo :: CustomerInfo
    , poShippingAddress :: Address
    , poBillingAddress :: Address
    , poAmountToBill :: BillingAmount
    , poLines :: [PricedOrderLine]
    , poPricingMethod :: PricingMethod
    } deriving (Eq, Show)

type PriceOrder
    =  GetPricingFunction     -- dependency
    -> ValidatedOrder  -- input
    -> Either PricingLeft PricedOrder   -- output

-- ---------------------------
-- Shipping
-- ---------------------------

data ShippingMethod =
    PostalService
    | Fedex24
    | Fedex48
    | Ups48 deriving (Eq, Show)

data ShippingInfo = ShippingInfo {
    siShippingMethod :: ShippingMethod
    , siShippingCost :: Price
    } deriving (Eq, Show)

data PricedOrderWithShippingMethod = PricedOrderWithShippingMethod {
    powsiShippingInfo :: ShippingInfo
    , powsiPricedOrder :: PricedOrder
    } deriving (Eq, Show)

type CalculateShippingCost = PricedOrder -> Price

type AddShippingInfoToOrder
    =  CalculateShippingCost -- dependency
    -> PricedOrder       -- input
    -> PricedOrderWithShippingMethod  -- output

-- ---------------------------
-- VIP shipping
-- ---------------------------

type FreeVipShipping
    = PricedOrderWithShippingMethod -> PricedOrderWithShippingMethod

-- ---------------------------
-- Send OrderAcknowledgment 
-- ---------------------------

data HtmlString =
    HtmlString String deriving (Eq, Show)

data OrderAcknowledgment = OrderAcknowledgment {
    oaEmailAddress :: EmailAddress
    , oaLetter :: HtmlString
    } deriving (Eq, Show)

type CreateOrderAcknowledgmentLetter
    = PricedOrderWithShippingMethod -> HtmlString

-- Send the order acknowledgement to the customer
-- Note that this does NOT generate an Result-type error (at least not in this workflow)
-- because on failure we will continue anyway.
-- On success, we will generate a OrderAcknowledgmentSent event,
-- but on failure we won't.

data SendResult = Sent | NotSent deriving (Eq, Show)

type SendOrderAcknowledgment = OrderAcknowledgment -> SendResult

type AcknowledgeOrder
    =  CreateOrderAcknowledgmentLetter  -- dependency
    -> SendOrderAcknowledgment      -- dependency
    -> PricedOrderWithShippingMethod  -- input
    -> Maybe OrderAcknowledgmentSent -- output

-- ---------------------------
-- Create events
-- ---------------------------

type CreateEvents
    =  PricedOrder                           -- input
    -> Maybe OrderAcknowledgmentSent    -- input (event from previous step)
    -> [PlaceOrderEvent]              -- output

