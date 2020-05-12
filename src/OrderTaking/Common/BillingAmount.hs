module OrderTaking.Common.BillingAmount
    ( BillingAmount()
    , OrderTaking.Common.BillingAmount.create
    , OrderTaking.Common.BillingAmount.value
    , OrderTaking.Common.BillingAmount.sumPrices
    )
where

import           OrderTaking.Common.ConstrainedType
import           OrderTaking.Common.Result
import           OrderTaking.Common.Price as Price
import           Data.List as List

-- Constrained to be a integer between 1 and 10000
data BillingAmount = BillingAmountContent Double deriving (Eq, Show)

-- Return the value inside a BillingAmount 
value :: BillingAmount -> Double
value (BillingAmountContent v) = v

-- Create a BillingAmount from a decimal.
-- Return Error if input is not a decimal between 0.0 and 10000.00 
create :: Double -> Result BillingAmount
create = createDecimal "BillingAmount" BillingAmountContent 0.0 10000.0


-- Sum a list of prices to make a billing amount
-- Return Error if total is out of bounds
sumPrices :: [Price] -> Result BillingAmount
sumPrices = OrderTaking.Common.BillingAmount.create . List.sum . (List.map Price.value)
-- sumPrices prices = prices >> (List.map Price.value) >> List.sum >> OrderTaking.Common.BillingAmount.create
