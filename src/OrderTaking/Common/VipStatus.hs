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
-- Return Left if input is null, empty, or doesn't match one of the cases
create :: String -> String -> Either ErrorMsg VipStatus
create _         "normal" = Right Normal
create _         "Normal" = Right Normal
create _         "vip"    = Right Vip
create _         "VIP"    = Right Vip
create fieldName _ = Left $ fieldName ++ ": Must be one of 'Normal', 'VIP'"
