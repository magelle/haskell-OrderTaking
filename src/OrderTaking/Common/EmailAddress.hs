module OrderTaking.Common.EmailAddress
    ( create
    , value
    )
where

import           OrderTaking.Common.Result
import           OrderTaking.Common.ConstrainedType

-- An email address
data EmailAddress = EmailAddressContent String deriving (Eq, Show)

-- Return the string value inside an EmailAddress 
value :: EmailAddress -> String
value (EmailAddressContent str) = str

-- Create an EmailAddress from a string
-- Return Error if input is null, empty, or doesn't have an "@" in it
create :: String -> String -> Result EmailAddress
create fieldName = createLike fieldName EmailAddressContent "^.+@.+$"
