module OrderTaking.Common.OrderId
    ( OrderId()
    , create
    , value
    )
where

import           OrderTaking.Common.ConstrainedType
import           OrderTaking.Common.Result

-- An Id for Orders. Constrained to be a non-empty string < 10 chars
data OrderId = MkOrderId String deriving (Eq, Show)

-- Return the string value inside an OrderId
value :: OrderId -> String
value (MkOrderId str) = str

-- Create an OrderId from a string
-- Return Left if input is null, empty, or length > 50
create :: String -> String -> Either ErrorMsg OrderId
create fieldName = createString fieldName MkOrderId 50



