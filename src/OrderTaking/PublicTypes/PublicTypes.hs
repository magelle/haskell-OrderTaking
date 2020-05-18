module OrderTaking.PublicTypes.PublicTypes
    ( UnvalidatedCustomerInfo(..)
    , UnvalidatedAddress(..)
    , UnvalidatedOrderLine(..)
    , UnvalidatedOrder(..)
    , OrderAcknowledgmentSent(..)
    , ShippableOrderLine(..)
    , ShippableOrderPlaced(..)
    , BillableOrderPlaced(..)
    , PlaceOrderEvent(..)
    , ValidationLeft(..)
    , PricingLeft(..)
    , ServiceInfo(..)
    , RemoteServiceLeft(..)
    , PlaceOrderLeft(..)
    , PlaceOrder
    )
where

import           OrderTaking.Common.OrderId
import           OrderTaking.Common.EmailAddress
import           OrderTaking.Common.ProductCode
import           OrderTaking.Common.OrderQuantity
import           OrderTaking.Common.PdfAttachment
import           OrderTaking.Common.BillingAmount
import           OrderTaking.Common.Result
import           OrderTaking.CompoundTypes.CustomerInfo
import           OrderTaking.CompoundTypes.Address

-- -- ==================================
-- -- This file contains the definitions of PUBLIC datas (exposed at the boundary of the bounded context)
-- -- related to the PlaceOrder workflow 
-- -- ==================================


-- -- ==================================
-- -- PlaceOrder workflow
-- -- ==================================

-- -- ------------------------------------
-- -- inputs to the workflow

data UnvalidatedCustomerInfo = UnvalidatedCustomerInfo
    {
    uciFirstName :: String
    , uciLastName :: String
    , uciEmailAddress :: String
    , uciVipStatus :: String
    } deriving (Eq, Show)

data UnvalidatedAddress = UnvalidatedAddress {
    uaAddressLine1 :: String
    , uaAddressLine2 :: String
    , uaAddressLine3 :: String
    , uaAddressLine4 :: String
    , uaCity :: String
    , uaZipCode :: String
    , uaState :: String
    , uaCountry :: String
    } deriving (Eq, Show)

data UnvalidatedOrderLine = UnvalidatedOrderLine {
    ualOrderLineId :: String
    , ualProductCode :: String
    , ualQuantity :: Double
    } deriving (Eq, Show)

data UnvalidatedOrder = UnvalidatedOrder {
    uoOrderId :: String
    , uoCustomerInfo :: UnvalidatedCustomerInfo
    , uoShippingAddress :: UnvalidatedAddress
    , uoSillingAddress :: UnvalidatedAddress
    , uoLines :: [UnvalidatedOrderLine]
    , uoPromotionCode :: String
    } deriving (Eq, Show)


-- -- ------------------------------------
-- -- outputs from the workflow (success case)

-- -- Event will be created if the Acknowledgment was successfully posted
data OrderAcknowledgmentSent = OrderAcknowledgmentSent {
    oasOrderId :: OrderId
    , oasEmailAddress :: EmailAddress
    } deriving (Eq, Show)



-- -- Event to send to shipping context
-- type OrderPlaced = PricedOrder

data ShippableOrderLine = ShippableOrderLine {
    solProductCode :: ProductCode
    , solQuantity :: OrderQuantity
    } deriving (Eq, Show)

data ShippableOrderPlaced = ShippableOrderPlaced {
    spoOrderId :: OrderId
    , spoShippingAddress :: Address
    , spoShipmentLines :: [ShippableOrderLine]
    , spoPdf :: PdfAttachment
} deriving (Eq, Show)

-- -- Event to send to billing context
-- -- Will only be created if the AmountToBill is not zero
data BillableOrderPlaced = BillableOrderPlaced {
    bopOrderId :: OrderId
    , bopBillingAddress :: Address
    , bopAmountToBill :: BillingAmount
    } deriving (Eq, Show)

-- -- The possible events resulting from the PlaceOrder workflow
-- -- Not all events will occur, depending on the logic of the workflow
data PlaceOrderEvent = PoeShippableOrderPlaced ShippableOrderPlaced | PoeBillableOrderPlaced BillableOrderPlaced | PoeAcknowledgmentSent OrderAcknowledgmentSent deriving (Eq, Show)



-- -- ------------------------------------
-- -- error outputs 


-- -- All the things that can go wrong in this workflow
data ValidationLeft = ValidationLeft String deriving (Eq, Show)

data PricingLeft = PricingLeft String deriving (Eq, Show)

data ServiceInfo = ServiceInfo {
    siName :: String
    , siEndpoint :: String -- System.Uri
    } deriving (Eq, Show)

data RemoteServiceLeft = RemoteServiceLeft {
    rseService :: ServiceInfo
    , rseException :: String -- System.Exception
    } deriving (Eq, Show)

data PlaceOrderLeft =
    Validation ValidationLeft
    | Pricing PricingLeft
    | RemoteService RemoteServiceLeft deriving (Eq, Show)

-- -- ------------------------------------
-- -- the workflow itself

type PlaceOrder
    = UnvalidatedOrder -> IOResult PlaceOrderLeft [PlaceOrderEvent]
