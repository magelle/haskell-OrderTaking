module OrderTaking.Common.GizmoCode
    ( GizmoCode()
    , create
    , value
    )
where

import           OrderTaking.Common.Result
import           OrderTaking.Common.ConstrainedType

-- A zip code
data GizmoCode = MkGizmoCode String deriving (Eq, Show)

-- Return the string value inside a GizmoCode
value :: GizmoCode -> String
value (MkGizmoCode str) = str

-- Create an GizmoCode from a string
-- Return Left if input is null. empty, or not matching pattern
create :: String -> String -> Either ErrorMsg GizmoCode
create fieldName = createLike fieldName MkGizmoCode "^G[0-9]{4}$"