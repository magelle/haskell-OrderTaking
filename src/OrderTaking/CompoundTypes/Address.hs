module OrderTaking.CompoundTypes.Address
    ( Address(..)
    )
where

import           OrderTaking.Common.String50
import           OrderTaking.Common.ZipCode
import           OrderTaking.Common.UsStateCode

-- ==================================
-- Address-related
-- ==================================


data Address = MkAddress {
    addressLine1 :: String50
    ,addressLine2 :: Maybe String50
    ,addressLine3 :: Maybe String50
    ,addressLine4 :: Maybe String50
    ,city :: String50
    ,zipCode :: ZipCode
    ,state :: UsStateCode
    ,country :: String50
    } deriving (Eq, Show)
