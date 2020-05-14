module OrderTaking.CompoundTypes.PersonalName
    ( PersonalName(..)
    )
where

import           OrderTaking.Common.String50

-- ==================================
-- Customer-related types
-- ==================================

data PersonalName = MkPersonalName {
    firstName :: String50
    , lastName :: String50
} deriving (Eq, Show)
