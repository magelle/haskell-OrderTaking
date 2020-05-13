module OrderTaking.Common.OrderQuantity
    ( OrderQuantity()
    , OrderTaking.Common.OrderQuantity.create
    , OrderTaking.Common.OrderQuantity.value
    )
where

import           OrderTaking.Common.Result
import           OrderTaking.Common.ProductCode
import           OrderTaking.Common.UnitQuantity
                                               as UnitQuantity
import           OrderTaking.Common.KilogramQuantity
                                               as KilogramQuantity


-- A Quantity is either a Unit or a Kilogram
data OrderQuantity = Unit UnitQuantity | Kilogram KilogramQuantity deriving (Eq, Show)

-- Return the value inside a OrderQuantity  
value :: OrderQuantity -> Double
value (Unit     uq) = (fromIntegral . UnitQuantity.value) uq
value (Kilogram kq) = KilogramQuantity.value kq

-- Create a OrderQuantity from a productCode and quantity  
create :: String -> ProductCode -> Double -> Either ErrorMsg OrderQuantity
create fieldName (Widget _) = (fmap Unit) . (UnitQuantity.create fieldName) . round
create fieldName (Gizmo _) = (fmap Kilogram) . (KilogramQuantity.create fieldName)
