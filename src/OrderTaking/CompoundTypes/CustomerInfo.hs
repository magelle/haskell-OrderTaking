module OrderTaking.CompoundTypes.CustomerInfo
    ( CustomerInfo
    )
where

import           OrderTaking.CompoundTypes.PersonalName
import           OrderTaking.Common.EmailAddress
import           OrderTaking.Common.VipStatus

-- ==================================
-- Customer-related types
-- ==================================

data CustomerInfo = CustomerInfo {
    name :: PersonalName
    , emailAddress :: EmailAddress
    , vipStatus :: VipStatus
} deriving (Eq, Show)
