module OrderTaking.PublicTypes.PublicTypes
    (
UnvalidatedCustomerInfo(..)
,UnvalidatedAddress(..)
,UnvalidatedOrderLine(..)
,UnvalidatedOrder(..)
,OrderAcknowledgmentSent(..)
,ShippableOrderLine(..)
,ShippableOrderPlaced(..)
,BillableOrderPlaced(..)
,PlaceOrderEvent(..)
,ValidationError(..)
,PricingError(..)
,ServiceInfo(..)
,RemoteServiceError(..)
,PlaceOrderError(..)
,PlaceOrder
    )
where

import OrderTaking.Common.OrderId
import OrderTaking.Common.EmailAddress
import OrderTaking.Common.ProductCode
import OrderTaking.Common.OrderQuantity
import OrderTaking.Common.PdfAttachment
import OrderTaking.Common.BillingAmount
import OrderTaking.Common.Result
import OrderTaking.CompoundTypes.CustomerInfo
import OrderTaking.CompoundTypes.Address

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
    }

data UnvalidatedAddress = UnvalidatedAddress {
    uaAddressLine1 :: String
    , uaAddressLine2 :: String
    , uaAddressLine3 :: String
    , uaAddressLine4 :: String
    , uaCity :: String
    , uaZipCode :: String
    , uaState :: String
    , uaCountry :: String
    }

data UnvalidatedOrderLine = UnvalidatedOrderLine {
    ualOrderLineId :: String
    , ualProductCode :: String
    , ualQuantity :: Double
    }

data UnvalidatedOrder = UnvalidatedOrder {
    uoOrderId :: String
    , uoCustomerInfo :: UnvalidatedCustomerInfo
    , uoShippingAddress :: UnvalidatedAddress
    , uoSillingAddress :: UnvalidatedAddress
    , uoLines :: [UnvalidatedOrderLine]
    , uoPromotionCode :: String
    }


-- -- ------------------------------------
-- -- outputs from the workflow (success case)

-- -- Event will be created if the Acknowledgment was successfully posted
data OrderAcknowledgmentSent = OrderAcknowledgmentSent {
    oasOrderId :: OrderId
    , oasEmailAddress :: EmailAddress
    }



-- -- Event to send to shipping context
-- type OrderPlaced = PricedOrder

data ShippableOrderLine = ShippableOrderLine {
    solProductCode :: ProductCode
    , solQuantity :: OrderQuantity
    }

data ShippableOrderPlaced = ShippableOrderPlaced {
    spoOrderId :: OrderId
    , spoShippingAddress :: Address
    , spoShipmentLines :: [ShippableOrderLine]
    , spoPdf :: PdfAttachment
}

-- -- Event to send to billing context
-- -- Will only be created if the AmountToBill is not zero
data BillableOrderPlaced = BillableOrderPlaced {
    bopOrderId :: OrderId
    , bopBillingAddress :: Address
    , bopAmountToBill :: BillingAmount
    }

-- -- The possible events resulting from the PlaceOrder workflow
-- -- Not all events will occur, depending on the logic of the workflow
data PlaceOrderEvent = PoeShippableOrderPlaced ShippableOrderPlaced | PoeBillableOrderPlaced BillableOrderPlaced | PoeAcknowledgmentSent OrderAcknowledgmentSent



-- -- ------------------------------------
-- -- error outputs 


-- -- All the things that can go wrong in this workflow
data ValidationError = ValidationError String

data PricingError = PricingError String

data ServiceInfo = ServiceInfo {
    siName :: String
    , siEndpoint :: String -- System.Uri
    }

data RemoteServiceError = RemoteServiceError {
    rseService :: ServiceInfo
    , rseException :: String -- System.Exception
    }

data PlaceOrderError =
    Validation ValidationError
    | Pricing PricingError
    | RemoteService RemoteServiceError

-- -- ------------------------------------
-- -- the workflow itself

type PlaceOrder
    = UnvalidatedOrder -> AsyncResult [PlaceOrderEvent] PlaceOrderError
