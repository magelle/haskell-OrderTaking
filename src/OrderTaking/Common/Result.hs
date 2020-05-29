module OrderTaking.Common.Result
    ( ErrorMsg
    )
where

import           GHC.Base

type ErrorMsg = String

-- ==============================================
-- The `Validation` type is the same as the `Result` type but with a *list* for failures
-- rather than a single value. This allows `Validation` types to be combined
-- by combining their errors ("applicative-style")
-- ==============================================

-- ==============================================
-- Async utilities
-- ==============================================

-- ==============================================
-- AsyncResult
-- ==============================================
