module OrderTaking.CompoundTypes.PersonalName
    ( PersonalName(..)
    )
where

import           OrderTaking.Common.String50

-- ==================================
-- Customer-related types
-- ==================================

data PersonalName = PersonalName {
    firstName :: String50
    , lastName :: String50
}
