module OrderTaking.Common.Price
    ( Price()
    , create
    , value
    , multiply
    )
where

import           OrderTaking.Common.ConstrainedType
import           OrderTaking.Common.Result

-- Constrained to be a decimal between 0.0 and 1000.00 
data Price = MkPrice Double deriving (Eq, Show)

-- Return the value inside a Price 
value :: Price -> Double
value (MkPrice v) = v

-- Create a Price from a decimal.
-- Return Left if input is not a decimal between 0.0 and 1000.00  
create :: Double -> Either ErrorMsg Price
create = createDecimal "Price" MkPrice 0.0 1000.0

-- Create a Price from a decimal.
-- Throw an exception if out of bounds. This should only be used if you know the value is valid.
-- let unsafeCreate v = 
--     create v 
--     |> function
--         | Ok price -> 
--             price
--         | Left err -> 
--             failwithf "Not expecting Price to be out of bounds: %s" err

-- Multiply a Price by a decimal qty.
-- Return Left if new price is out of bounds.
multiply :: Double -> Price -> Either ErrorMsg Price
multiply qty (MkPrice p) = create (qty * p)