module OrderTaking.Common.KilogramQuantity
    ( KilogramQuantity()
    , create
    , value
    )
where

import           OrderTaking.Common.ConstrainedType
import           OrderTaking.Common.Result

-- Constrained to be a integer between 1 and 1000
data KilogramQuantity = MkKilogramQuantity Double deriving (Eq, Show)

-- Return the value inside a KilogramQuantity 
value :: KilogramQuantity -> Double
value (MkKilogramQuantity v) = v

-- Create a KilogramQuantity from a decimal
-- Return Error if input is not a decimal between 0.05 and 100.00 
create :: String -> Double -> Result KilogramQuantity String
create fieldName = createDecimal fieldName MkKilogramQuantity 0.05 100000.0
