module OrderTaking.Implementation.Implementation
    ( toCustomerInfo
    , toAddress
    , toOrderId
    , toOrderLineId
    , toProductCode
    , toCheckedAddress
    )
where

import           Flow
import           Data.List                     as List
import           Data.Either                   as Either
import           Data.Either.Combinators
import           Data.Bifunctor
import           Control.Monad                 as Monad
import           Control.Monad.Except          as Except
import           GHC.Base                      as Base
import           OrderTaking.Common.Result
import           OrderTaking.InternalTypes.InternalTypes
import           OrderTaking.PublicTypes.PublicTypes
import           OrderTaking.Common.Price      as Price
import           OrderTaking.Pricing.Pricing   as Pricing
import qualified OrderTaking.Common.String50   as String50
import qualified OrderTaking.Common.EmailAddress
                                               as EmailAddress
import qualified OrderTaking.Common.VipStatus  as VipStatus
import           OrderTaking.CompoundTypes.CustomerInfo
                                               as CustomerInfo
import           OrderTaking.CompoundTypes.Address
                                               as Address
import           OrderTaking.CompoundTypes.PersonalName
                                               as PersonalName

import qualified OrderTaking.Common.ZipCode    as ZipCode
import qualified OrderTaking.Common.UsStateCode
                                               as UsStateCode
import qualified OrderTaking.Common.OrderId    as OrderId
import qualified OrderTaking.Common.OrderLineId
                                               as OrderLineId
import qualified OrderTaking.Common.ProductCode
                                               as ProductCode
import qualified OrderTaking.Common.OrderQuantity
                                               as OrderQuantity
import qualified OrderTaking.Common.PromotionCode
                                               as PromotionCode

import qualified OrderTaking.Common.BillingAmount
                                               as BillingAmount 
-- ======================================================
-- This file contains the final implementation for the PlaceOrderWorkflow
-- 
-- This represents the code in chapter 10, "Working with Lefts"
-- 
-- There are two parts:
-- * the first section contains the (type-only) definitions for each step
-- * the second section contains the implementations for each step
--   and the implementation of the overall workflow
-- ======================================================



-- ======================================================
-- Section 2 : Implementation
-- ======================================================

-- ---------------------------
-- ValidateOrder step
-- -----------------------------

toCustomerInfo :: UnvalidatedCustomerInfo -> Either ErrorMsg CustomerInfo
toCustomerInfo unvalidatedCustomerInfo = do
    firstName <- String50.create "FirstName"
        $ uciFirstName unvalidatedCustomerInfo
    lastName <- String50.create "LastName" $ uciLastName unvalidatedCustomerInfo
    emailAddress <- EmailAddress.create "EmailAddress"
        $ uciEmailAddress unvalidatedCustomerInfo
    vipStatus <- VipStatus.create "vipStatus"
        $ uciVipStatus unvalidatedCustomerInfo
    Right CustomerInfo.MkCustomerInfo
        { name         = PersonalName.MkPersonalName { firstName = firstName
                                                     , lastName  = lastName
                                                     }
        , emailAddress = emailAddress
        , vipStatus    = vipStatus
        }

toAddress :: CheckedAddress -> Either ErrorMsg Address
toAddress (CheckedAddress unvalidatedAddress) = do
    addressLine1 <- String50.create "AddressLine1"
        $ uaAddressLine1 unvalidatedAddress
    addressLine2 <- String50.createOption "AddressLine2"
        $ uaAddressLine2 unvalidatedAddress
    addressLine3 <- String50.createOption "AddressLine3"
        $ uaAddressLine3 unvalidatedAddress
    addressLine4 <- String50.createOption "AddressLine4"
        $ uaAddressLine4 unvalidatedAddress
    city    <- String50.create "City" $ uaCity unvalidatedAddress
    zipCode <- ZipCode.create "ZipCode" $ uaZipCode unvalidatedAddress
    state   <- UsStateCode.create "State" $ uaState unvalidatedAddress
    country <- String50.create "Country" $ uaCountry unvalidatedAddress
    Right Address.MkAddress { addressLine1 = addressLine1
                            , addressLine2 = addressLine2
                            , addressLine3 = addressLine3
                            , addressLine4 = addressLine4
                            , city         = city
                            , zipCode      = zipCode
                            , state        = state
                            , country      = country
                            }


-- Call the checkAddressExists and convert the error to a ValidationLeft
toCheckedAddress
    :: CheckAddressExists
    -> UnvalidatedAddress
    -> IO (Either ErrorMsg CheckedAddress)
toCheckedAddress checkAddress address =
    fmap mapAddressValidationResult (checkAddress address)

mapAddressValidationLeft :: AddressValidationLeft -> ErrorMsg
mapAddressValidationLeft AddressNotFound = "Address not found"
mapAddressValidationLeft InvalidFormat   = "Address has bad format"

mapAddressValidationResult
    :: Either AddressValidationLeft CheckedAddress
    -> Either ErrorMsg CheckedAddress
mapAddressValidationResult (Right addr) = Right addr
mapAddressValidationResult (Left  err ) = Left $ mapAddressValidationLeft err


toOrderId :: String -> Either ErrorMsg OrderId.OrderId
toOrderId = OrderId.create "OrderId"

--  Helper function for validateOrder   
toOrderLineId :: String -> Either ErrorMsg OrderLineId.OrderLineId
toOrderLineId = OrderLineId.create "OrderLineId"

--  Helper function for validateOrder   
toProductCode
    :: CheckProductCodeExists
    -> String
    -> Either ErrorMsg ProductCode.ProductCode
toProductCode checkProductCodeExists productCode =
    let checkProduct pc = if checkProductCodeExists pc
            then Right pc
            else Left $ "Invalid: " ++ (show pc)
    in  ProductCode.create "ProductCode" productCode >>= checkProduct


--  Helper function for validateOrder   
toOrderQuantity
    :: ProductCode.ProductCode
    -> Double
    -> Either ErrorMsg OrderQuantity.OrderQuantity
toOrderQuantity = OrderQuantity.create "OrderQuantity"

--  Helper function for validateOrder   
toValidatedOrderLine
    :: CheckProductCodeExists
    -> UnvalidatedOrderLine
    -> Either ErrorMsg ValidatedOrderLine
toValidatedOrderLine checkProductExists unvalidatedOrderLine = do
    orderLineId <- toOrderLineId $ ualOrderLineId unvalidatedOrderLine
    productCode <- toProductCode checkProductExists
        $ ualProductCode unvalidatedOrderLine
    quantity <- toOrderQuantity productCode $ ualQuantity unvalidatedOrderLine
    Right ValidatedOrderLine { volOrderLineId = orderLineId
                             , volProductCode = productCode
                             , volQuantity    = quantity
                             }

validateOrder
    :: CheckProductCodeExists
    -> CheckAddressExists
    -> UnvalidatedOrder
    -> Except.ExceptT ErrorMsg IO ValidatedOrder
validateOrder checkProductCodeExists checkAddressExists unvalidatedOrder = do
    orderId <- unvalidatedOrder |> uoOrderId |> toOrderId |> Except.liftEither
    customerInfo <-
        unvalidatedOrder
        |> uoCustomerInfo
        |> toCustomerInfo
        |> Except.liftEither
    checkedShippingAddress <-
        unvalidatedOrder
        |> uoShippingAddress
        |> (toCheckedAddress checkAddressExists)
        |> Except.ExceptT
    shippingAddress <- checkedShippingAddress |> toAddress |> Except.liftEither
    checkedBillingAddress <-
        unvalidatedOrder
        |> uoBillingAddress
        |> (toCheckedAddress checkAddressExists)
        |> Except.ExceptT
    billingAddress <- checkedBillingAddress |> toAddress |> Except.liftEither
    lines          <-
        unvalidatedOrder
        |> uoLines
        |> (map (toValidatedOrderLine checkProductCodeExists))
        |> Monad.sequence
        |> Except.liftEither
    pricingMethod <-
        unvalidatedOrder
        |> uoPromotionCode
        |> Pricing.createPricingMethod
        |> Except.return
    Except.return ValidatedOrder { voOrderId         = orderId
                                 , voCustomerInfo    = customerInfo
                                 , voShippingAddress = shippingAddress
                                 , voBillingAddress  = billingAddress
                                 , voLines           = lines
                                 , voPricingMethod   = pricingMethod
                                 }

-- // ---------------------------
-- // PriceOrder step
-- // ---------------------------

toPricedOrderLine
    :: GetProductPrice
    -> ValidatedOrderLine
    -> Either PlaceOrderLeft PricedOrderLine
toPricedOrderLine getProductPrice validatedOrderLine = do
    qty <- validatedOrderLine |> volQuantity |> OrderQuantity.value |> Right
    price <- validatedOrderLine |> volProductCode |> getProductPrice |> Right
    linePrice <- Price.multiply qty price |> mapLeft Pricing
    pricedLine <- Right PricedOrderProductLine
        { poplOrderLineId = volOrderLineId validatedOrderLine
        , poplProductCode = volProductCode validatedOrderLine
        , poplQuantity    = volQuantity validatedOrderLine
        , poplLinePrice   = linePrice
        }
    Right $ ProductLine pricedLine

-- add the special comment line if needed
addCommentLine :: PricingMethod -> [PricedOrderLine] -> [PricedOrderLine]
addCommentLine Standard lines = lines
addCommentLine (Promotion (PromotionCode.MkPromotionCode promoCode)) lines =
    let appliedPromotion = ("Applied promotion " ++ promoCode) |> CommentLine
    in  lines ++ [appliedPromotion]

getLinePrice :: PricedOrderLine -> Price
getLinePrice (ProductLine line) = poplLinePrice line
getLinePrice (CommentLine _   ) = Price.unsafeCreate 0

-- Extracted out to get the right type
validateLines :: GetPricingFunction -> ValidatedOrder -> Either PlaceOrderLeft [PricedOrderLine]
validateLines getPricingFunction validatedOrder = 
    let getProductPrice = validatedOrder |> voPricingMethod |> getPricingFunction
    in validatedOrder
        |> voLines
        |> map (toPricedOrderLine getProductPrice)
        |> Monad.sequence
        |> fmap (addCommentLine (voPricingMethod validatedOrder))

-- Extracted out to get the right type
sumAmountToBill :: [PricedOrderLine] -> Either PlaceOrderLeft BillingAmount.BillingAmount
sumAmountToBill pricedOrderLines = 
    pricedOrderLines
        |> (map getLinePrice)
        |> BillingAmount.sumPrices
        |> (first Pricing)

priceOrder :: GetPricingFunction -> ValidatedOrder -> Either PlaceOrderLeft PricedOrder
priceOrder getPricingFunction validatedOrder =
    do
        lines <-validateLines getPricingFunction validatedOrder 
        amountToBill <- sumAmountToBill lines
        Right PricedOrder {
            poOrderId = voOrderId validatedOrder
            , poCustomerInfo = voCustomerInfo validatedOrder
            , poShippingAddress = voShippingAddress validatedOrder
            , poBillingAddress = voBillingAddress validatedOrder 
            , poAmountToBill = amountToBill
            , poLines = lines
            , poPricingMethod = voPricingMethod validatedOrder
        }

-- // ---------------------------
-- // Shipping step
-- // ---------------------------

data ShippingDistance =
    UsLocalState
    | UsRemoteState
    | International deriving (Eq, Show)

toShippingDistance :: Address -> ShippingDistance
toShippingDistance address
    | "US" == (address |> Address.country |> String50.value) =  
        case UsStateCode.value (Address.state address) of
           "CA" -> UsLocalState
           "OR" -> UsLocalState
           "AZ" -> UsLocalState
           "NV" -> UsLocalState
           _    -> UsRemoteState
    | otherwise = International


calculateShippingCost :: CalculateShippingCost
calculateShippingCost pricedOrder = 
    let 
        shippingDistance = pricedOrder |> poShippingAddress |> toShippingDistance
        shippingRate = case shippingDistance of
            UsLocalState ->  5.0
            UsRemoteState -> 10.0
            International -> 20.0
    in 
        shippingRate |> Price.unsafeCreate


addShippingInfoToOrder :: AddShippingInfoToOrder
addShippingInfoToOrder calculateShippingCost pricedOrder = 
    let 
        shippingInfo = ShippingInfo Fedex24 (calculateShippingCost pricedOrder)
    in 
        PricedOrderWithShippingMethod {
            powsiShippingInfo = shippingInfo
            , powsiPricedOrder = pricedOrder
        }

-- // ---------------------------
-- // VIP shipping step
-- // ---------------------------

vipStatus :: PricedOrderWithShippingMethod -> VipStatus
vipStatus pricedOrder = 
    pricedOrder
    |> PricedOrderWithShippingMethod.powsiPricedOrder
    |> PricedOrder.poCustomerInfo
    |> CustomerInfo.vipStatus

freeVipShipping :: FreeVipShipping
freeVipShipping order = 
    let updatedShippingInfo = case (vipStatus order) of
            VipStatus.Normal -> PricedOrderWithShippingMethod.powsiShippingInfo order
            VipStatus.Vip -> ShippingInfo {
                    siShippingCost = Price.unsafeCreate  0.0
                    , siShippingMethod = Fedex24
                }
    in
        order where powsiShippingInfo = updatedShippingInfo

--  Update the shipping cost if customer is VIP
-- let freeVipShipping : FreeVipShipping =
--     fun order -> 
--         let updatedShippingInfo = 
--             match order.PricedOrder.CustomerInfo.VipStatus with
--             | Normal -> 
--                 // untouched
--                 order.ShippingInfo
--             | Vip -> 
--                 {order.ShippingInfo with 
--                     ShippingCost = Price.unsafeCreate 0.0M
--                     ShippingMethod = Fedex24 }

--         {order with ShippingInfo = updatedShippingInfo }


-- // ---------------------------
-- // AcknowledgeOrder step
-- // ---------------------------

-- let acknowledgeOrder : AcknowledgeOrder = 
--     fun createAcknowledgmentLetter sendAcknowledgment pricedOrderWithShipping ->
--         let pricedOrder = pricedOrderWithShipping.PricedOrder

--         let letter = createAcknowledgmentLetter pricedOrderWithShipping
--         let acknowledgment = {
--             EmailAddress = pricedOrder.CustomerInfo.EmailAddress
--             Letter = letter 
--             }


--         // if the acknowledgement was successfully sent,
--         // return the corresponding event, else return None
--         match sendAcknowledgment acknowledgment with
--         | Sent -> 
--             let event = {
--                 OrderId = pricedOrder.OrderId
--                 EmailAddress = pricedOrder.CustomerInfo.EmailAddress
--                 } 
--             Some event
--         | NotSent ->
--             None

-- // ---------------------------
-- // Create events
-- // ---------------------------

-- let makeShipmentLine (line: PricedOrderLine) : ShippableOrderLine option =
--     match line with
--     | ProductLine line ->
--         {
--         ProductCode = line.ProductCode
--         Quantity = line.Quantity
--         } |> Some
--     | CommentLine _ ->
--         None


-- let createShippingEvent (placedOrder:PricedOrder) : ShippableOrderPlaced =
--     {
--     OrderId = placedOrder.OrderId
--     ShippingAddress = placedOrder.ShippingAddress
--     ShipmentLines = placedOrder.Lines |> List.choose makeShipmentLine
--     Pdf = 
--         { 
--         Name = sprintf "Order%s.pdf" (placedOrder.OrderId |> OrderId.value)
--         Bytes = [||]
--         }
--     } 

-- let createBillingEvent (placedOrder:PricedOrder) : BillableOrderPlaced option =
--     let billingAmount = placedOrder.AmountToBill |> BillingAmount.value
--     if billingAmount > 0M then
--         {
--         OrderId = placedOrder.OrderId
--         BillingAddress = placedOrder.BillingAddress
--         AmountToBill = placedOrder.AmountToBill 
--         } |> Some
--     else
--         None

--  helper to convert an Option into a List
-- let listOfOption opt =
--     match opt with 
--     | Some x -> [x]
--     | None -> []

-- let createEvents : CreateEvents = 
--     fun pricedOrder acknowledgmentEventOpt ->
--         let acknowledgmentEvents = 
--             acknowledgmentEventOpt 
--             |> Option.map PlaceOrderEvent.AcknowledgmentSent
--             |> listOfOption
--         let shippingEvents = 
--             pricedOrder
--             |> createShippingEvent
--             |> PlaceOrderEvent.ShippableOrderPlaced
--             |> List.singleton
--         let billingEvents = 
--             pricedOrder
--             |> createBillingEvent 
--             |> Option.map PlaceOrderEvent.BillableOrderPlaced
--             |> listOfOption

--         // return all the events
--         [
--         yield! acknowledgmentEvents
--         yield! shippingEvents 
--         yield! billingEvents
--         ]            


-- // ---------------------------
-- // overall workflow
-- // ---------------------------

-- let placeOrder 
--     checkProductExists // dependency
--     checkAddressExists // dependency
--     getProductPrice    // dependency
--     calculateShippingCost // dependency
--     createOrderAcknowledgmentLetter  // dependency
--     sendOrderAcknowledgment // dependency
--     : PlaceOrder =       // definition of function

--     fun unvalidatedOrder -> 
--         asyncResult {
--             let! validatedOrder = 
--                 validateOrder checkProductExists checkAddressExists unvalidatedOrder 
--                 |> AsyncResult.mapLeft PlaceOrderLeft.Validation
--             let! pricedOrder = 
--                 priceOrder getProductPrice validatedOrder 
--                 |> AsyncResult.ofResult
--                 |> AsyncResult.mapLeft PlaceOrderLeft.Pricing
--             let pricedOrderWithShipping = 
--                 pricedOrder 
--                 |> addShippingInfoToOrder calculateShippingCost
--                 |> freeVipShipping
--             let acknowledgementOption = 
--                 acknowledgeOrder createOrderAcknowledgmentLetter sendOrderAcknowledgment pricedOrderWithShipping 
--             let events = 
--                 createEvents pricedOrder acknowledgementOption 
--             return events
--         }
