module OrderTaking.Common.ZipCode
    ( create
    , value
    )
where

import           OrderTaking.Common.Result
import           OrderTaking.Common.ConstrainedType

-- A zip code
data ZipCode = ZipCodeContent String deriving (Eq, Show)

-- Return the string value inside a ZipCode
value :: ZipCode -> String
value (ZipCodeContent str) = str

-- Create a ZipCode from a string
-- Return Error if input is null, empty, or doesn't have 5 digits
create :: String -> String -> Result ZipCode
create fieldName = createLike fieldName ZipCodeContent "^[0-9]{5}$"