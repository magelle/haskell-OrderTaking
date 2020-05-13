module OrderTaking.Common.WidgetCode
    ( WidgetCode
    , create
    , value
    )
where

import           OrderTaking.Common.Result
import           OrderTaking.Common.ConstrainedType

-- A zip code
data WidgetCode = MkWidgetCode String deriving (Eq, Show)

-- Return the string value inside a WidgetCode
value :: WidgetCode -> String
value (MkWidgetCode str) = str

-- Create an WidgetCode from a string
-- Return Left if input is null. empty, or not matching pattern
create :: String -> String -> Either ErrorMsg WidgetCode
create fieldName = createLike fieldName MkWidgetCode "^W[0-9]{4}$"