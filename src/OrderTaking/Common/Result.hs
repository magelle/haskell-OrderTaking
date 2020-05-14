module OrderTaking.Common.Result
    ( ErrorMsg
    , AsyncResult(..)
    , mapError
    )
where

import           Data.Either
import qualified Data.Validation

type ErrorMsg = String
mapError :: Either e a -> Either [e] a
mapError (Left e) = Left [e]
mapError (Right a) = Right a

-- ==============================================
-- The `Validation` type is the same as the `Result` type but with a *list* for failures
-- rather than a single value. This allows `Validation` types to be combined
-- by combining their errors ("applicative-style")
-- ==============================================

-- ==============================================
-- Async utilities
-- ==============================================

data Async val = Async val

-- ==============================================
-- AsyncResult
-- ==============================================

type AsyncResult value err = Async (Either err value)

