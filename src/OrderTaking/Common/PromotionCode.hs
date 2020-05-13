module OrderTaking.Common.PromotionCode
    ( PromotionCode(..)
    )
where

data PromotionCode = MkPromotionCode String deriving (Eq, Show)
