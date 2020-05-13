module OrderTaking.Common.String50
    ( String50()
    , create
    , createOption
    , value
    )
where

import           OrderTaking.Common.ConstrainedType
import           OrderTaking.Common.Result

-- Constrained to be 50 chars or less, not null
data String50 = String50Content String deriving (Eq, Show)

-- Return the value inside a String50
value :: String50 -> String
value (String50Content str) = str

-- Create an String50 from a string
-- Return Left if input is null, empty, or length > 50
create :: String -> String -> Either ErrorMsg String50
create fieldName = createString fieldName String50Content 50

-- Create an String50 from a string
-- Return None if input is null, empty. 
-- Return error if length > maxLen
-- Return Some if the input is valid
createOption :: String -> String -> Either ErrorMsg (Maybe String50)
createOption fieldName = createStringOption fieldName String50Content 50