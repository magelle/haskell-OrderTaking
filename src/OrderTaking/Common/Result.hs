module OrderTaking.Common.Result (
    Result (..),
    mapR,
    bimap,
    isOk, 
    isError,
    isR
) where

data Result a = Ok a | Error String deriving (Eq, Show)

-- Pass in a function to handle each case of `Result`
mapR :: (a -> b) -> Result a -> Result b
mapR f (Ok a) = Ok (f a)
mapR f (Error msg) = Error msg

bimap :: (a -> b) -> (String -> String) -> Result a -> Result b
bimap onSuccess onError (Ok a) = Ok $ onSuccess a
bimap onSuccess onError (Error err) = Error $ onError err

-------------------------------------
-- Predicates

--- Predicate that returns true on success
isOk :: Result a -> Bool
isOk (Ok _) = True 
isOk (Error _) = False  

-- Predicate that returns true on failure
isError :: Result a -> Bool
isError = not . isOk

-- Lift a given predicate into a predicate that works on Results
isR :: (a -> Bool) -> Result a -> Bool
isR pred (Ok a) = pred a 
isR pred (Error _) = False 
