module OrderTaking.Common.OrderId
    ( OrderId()
    , create
    , value
    )
where

import           OrderTaking.Common.ConstrainedType
import           OrderTaking.Common.Result

-- An Id for Orders. Constrained to be a non-empty string < 10 chars
data OrderId = OrderIdContent String deriving (Eq, Show)

-- Return the string value inside an OrderId
value :: OrderId -> String
value (OrderIdContent str) = str

-- Create an OrderId from a string
-- Return Error if input is null, empty, or length > 50
create :: String -> String -> Result OrderId
create fieldName = createString fieldName OrderIdContent 50



