module OrderTaking.Common.ZipCode
    ( ZipCode()
    , create
    , value
    )
where

import           OrderTaking.Common.Result
import           OrderTaking.Common.ConstrainedType

-- A zip code
data ZipCode = MkZipCode String deriving (Eq, Show)

-- Return the string value inside a ZipCode
value :: ZipCode -> String
value (MkZipCode str) = str

-- Create a ZipCode from a string
-- Return Left if input is null, empty, or doesn't have 5 digits
create :: String -> String -> Either ErrorMsg ZipCode
create fieldName = createLike fieldName MkZipCode "^[0-9]{5}$"