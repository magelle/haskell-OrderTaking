module OrderTaking.Common.GizmoCode
    ( GizmoCode()
    , create
    , value
    )
where

import           OrderTaking.Common.Result
import           OrderTaking.Common.ConstrainedType

-- A zip code
data GizmoCode = GizmoCodeContent String deriving (Eq, Show)

-- Return the string value inside a GizmoCode
value :: GizmoCode -> String
value (GizmoCodeContent str) = str

-- Create an GizmoCode from a string
-- Return Error if input is null. empty, or not matching pattern
create :: String -> String -> Result GizmoCode
create fieldName = createLike fieldName GizmoCodeContent "^G[0-9]{4}$"