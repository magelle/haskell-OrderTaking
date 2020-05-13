module OrderTaking.Common.EmailAddress
    ( EmailAddress()
    , create
    , value
    )
where

import           OrderTaking.Common.Result
import           OrderTaking.Common.ConstrainedType

-- An email address
data EmailAddress = MkEmailAddress String deriving (Eq, Show)

-- Return the string value inside an EmailAddress 
value :: EmailAddress -> String
value (MkEmailAddress str) = str

-- Create an EmailAddress from a string
-- Return Left if input is null, empty, or doesn't have an "@" in it
create :: String -> String -> Either ErrorMsg EmailAddress
create fieldName = createLike fieldName MkEmailAddress "^.+@.+$"
