module OrderTaking.Common.UnitQuantity
    ( UnitQuantity()
    , create
    , value
    )
where

import           OrderTaking.Common.ConstrainedType
import           OrderTaking.Common.Result

-- Constrained to be a integer between 1 and 1000
data UnitQuantity = MkUnitQuantity Int deriving (Eq, Show)

-- Return the value inside a UnitQuantity 
value :: UnitQuantity -> Int
value (MkUnitQuantity v) = v

-- Create a UnitQuantity from a int
-- Return Left if input is not an integer between 1 and 1000
create :: String -> Int -> Either ErrorMsg UnitQuantity
create fieldName = createInt fieldName MkUnitQuantity 1 1000
