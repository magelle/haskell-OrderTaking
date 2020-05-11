module OrderTaking.Common.WidgetCode
    ( WidgetCode
    , create
    , value
    )
where

import           OrderTaking.Common.Result
import           OrderTaking.Common.ConstrainedType

-- A zip code
data WidgetCode = WidgetCodeContent String deriving (Eq, Show)

-- Return the string value inside a WidgetCode
value :: WidgetCode -> String
value (WidgetCodeContent str) = str

-- Create an WidgetCode from a string
-- Return Error if input is null. empty, or not matching pattern
create :: String -> String -> Result WidgetCode
create fieldName = createLike fieldName WidgetCodeContent "^W[0-9]{4}$"