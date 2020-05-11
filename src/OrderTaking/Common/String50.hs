module OrderTaking.Common.String50
(
    String50 (),
    create, value
) where

import OrderTaking.Common.ConstrainedType
import OrderTaking.Common.Result

-- Constrained to be 50 chars or less, not null
data String50 = String50Content String deriving (Eq, Show)

-- Return the value inside a String50
value :: String50 -> String
value (String50Content str) = str

-- Create an String50 from a string
-- Return Error if input is null, empty, or length > 50
create :: String -> String -> Result String50    
create fieldName = createString fieldName String50Content 50

-- module String50 =

--     /// Return the value inside a String50
--     let value (String50 str) = str

--     /// Create an String50 from a string
--     /// Return Error if input is null, empty, or length > 50
--     let create fieldName str = 
--         ConstrainedType.createString fieldName String50 50 str

--     /// Create an String50 from a string
--     /// Return None if input is null, empty. 
--     /// Return error if length > maxLen
--     /// Return Some if the input is valid
--     let createOption fieldName str = 
--         ConstrainedType.createStringOption fieldName String50 50 str