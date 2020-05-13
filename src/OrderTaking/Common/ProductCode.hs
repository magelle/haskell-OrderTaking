module OrderTaking.Common.ProductCode
    ( ProductCode(Widget, Gizmo)
    , OrderTaking.Common.ProductCode.create
    , OrderTaking.Common.ProductCode.value
    )
where

import           OrderTaking.Common.ConstrainedType
import           OrderTaking.Common.Result
import           OrderTaking.Common.WidgetCode as WidgetCode
import           OrderTaking.Common.GizmoCode as GizmoCode

-- A ProductCode is either a Widget or a Gizmo
data ProductCode = Widget WidgetCode | Gizmo GizmoCode deriving (Eq, Show)

-- Return the string value inside a ProductCode 
value :: ProductCode -> String
value (Widget wc) = WidgetCode.value wc
value (Gizmo gc) = GizmoCode.value gc

-- Create an ProductCode from a string
-- Return Left if input is null, empty, or not matching pattern
create :: String -> String -> Either ErrorMsg ProductCode
create fieldName "" = Left $ fieldName ++ ": Must not be null or empty"
create fieldName code@('W':_) = fmap Widget (WidgetCode.create fieldName code)
create fieldName code@('G':_) = fmap Gizmo (GizmoCode.create fieldName code)
create fieldName code = Left $ fieldName ++ ": Format not recognized '" ++ code ++ "'"
