module OrderTaking.Common.Result (
    Result (..),
    mapR,
    bimap,
    isOk, 
    isError,
    isR,
    Validation (..),
    AsyncResult (..)
) where

-- ==============================================
-- Helpers for Result type and AsyncResult type
-- ==============================================

data Result value err = Ok value | Error err deriving (Eq, Show)

-- Pass in a function to handle each case of `Result`
mapR :: (value -> value') -> Result value err -> Result value' err
mapR f (Ok value) = Ok (f value)
mapR f (Error msg) = Error msg

bimap :: (value -> value') -> (err -> err') -> Result value err -> Result value' err'
bimap onSuccess onError (Ok value) = Ok $ onSuccess value
bimap onSuccess onError (Error msg) = Error $ onError msg

-------------------------------------
-- Predicates

--- Predicate that returns true on success
isOk :: Result value err -> Bool
isOk (Ok _) = True 
isOk (Error _) = False  

-- Predicate that returns true on failure
isError :: Result value err -> Bool
isError = not . isOk

-- Lift a given predicate into a predicate that works on Results
isR :: (value -> Bool) -> Result value err -> Bool
isR pred (Ok a) = pred a 
isR pred (Error _) = False 

-- ==============================================
-- The `Validation` type is the same as the `Result` type but with a *list* for failures
-- rather than a single value. This allows `Validation` types to be combined
-- by combining their errors ("applicative-style")
-- ==============================================

type Validation value err = Result value [err]

-- ==============================================
-- Async utilities
-- ==============================================

data Async val = Async val

-- ==============================================
-- AsyncResult
-- ==============================================

type AsyncResult value err = Async (Result value err)
