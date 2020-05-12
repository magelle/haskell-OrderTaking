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
-- Return Error if input is null, empty, or not matching pattern
create :: String -> String -> Result ProductCode String
create fieldName "" = Error $ fieldName ++ ": Must not be null or empty"
create fieldName code@('W':_) = mapR Widget (WidgetCode.create fieldName code)
create fieldName code@('G':_) = mapR Gizmo (GizmoCode.create fieldName code)
create fieldName code = Error $ fieldName ++ ": Format not recognized '" ++ code ++ "'"
