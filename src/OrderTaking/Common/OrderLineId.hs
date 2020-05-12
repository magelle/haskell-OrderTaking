module OrderTaking.Common.OrderLineId
    ( OrderLineId()
    , create
    , value
    )
where

import           OrderTaking.Common.ConstrainedType
import           OrderTaking.Common.Result

-- An Id for Orders. Constrained to be a non-empty string < 10 chars
data OrderLineId = MkOrderLineId String deriving (Eq, Show)

-- Return the string value inside an OrderLineId
value :: OrderLineId -> String
value (MkOrderLineId str) = str

-- Create an OrderLineId from a string
-- Return Error if input is null, empty, or length > 50
create :: String -> String -> Result OrderLineId String
create fieldName = createString fieldName MkOrderLineId 50



