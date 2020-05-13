module OrderTaking.Util
    ( isBlank
    )
where

import           Data.Char

isBlank :: String -> Bool
isBlank = all isSpace
