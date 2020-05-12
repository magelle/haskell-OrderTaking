module OrderTaking.Common.VipStatus
    ( VipStatus(Normal, Vip)
    , create
    , value
    )
where

import           OrderTaking.Common.Result

-- Customer's VIP status
data VipStatus = Normal | Vip  deriving (Eq, Show)

-- Return a string representation of VipStatus 
value :: VipStatus -> String
value Normal = "Normal"
value Vip    = "VIP"

-- Create a VipStatus from a string
-- Return Error if input is null, empty, or doesn't match one of the cases
create :: String -> String -> Result VipStatus String
create _         "normal" = Ok Normal
create _         "Normal" = Ok Normal
create _         "vip"    = Ok Vip
create _         "VIP"    = Ok Vip
create fieldName _ = Error $ fieldName ++ ": Must be one of 'Normal', 'VIP'"
