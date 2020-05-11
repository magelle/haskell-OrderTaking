module OrderTaking.Common.ConstrainedType
    ( createString
    , createStringOption
    , createInt
    , createLike
    )
where

import           OrderTaking.Common.Regex
import           OrderTaking.Common.Result

-- ===============================
-- Reusable constructors and getters for constrained types
-- ===============================
--     Create a constrained string using the constructor provided
--     Return Error if input is null, empty, or length > maxLen
createString :: String -> (String -> a) -> Int -> String -> Result a
createString fieldName constructor maxLen str
    | length str == 0
    = Error $ fieldName ++ " must not be empty"
    | length str > maxLen
    = Error
        $  fieldName
        ++ " must not be more than "
        ++ (show maxLen)
        ++ " chars"
    | otherwise
    = Ok $ constructor str

--     Create a optional constrained string using the constructor provided
--     Return None if input is null, empty. 
--     Return error if length > maxLen
--     Return Some if the input is valid
createStringOption
    :: String -> (String -> a) -> Int -> String -> Result (Maybe a)
createStringOption fieldName constructor maxLen str
    | length str == 0
    = Ok Nothing
    | length str > maxLen
    = Error
        $  fieldName
        ++ " must not be more than "
        ++ (show maxLen)
        ++ " chars"
    | otherwise
    = Ok (Just $ constructor str)

--     Create a constrained integer using the constructor provided
--     Return Error if input is less than minVal or more than maxVal
createInt :: String -> (Int -> a) -> Int -> Int -> Int -> Result a
createInt fieldName constructor minVal maxVal i
    | i < minVal
    = Error $ fieldName ++ ": Must not be less than " ++ (show minVal)
    | i > maxVal
    = Error $ fieldName ++ ": Must not be greater than " ++ (show maxVal)
    | otherwise
    = Ok (constructor i)

--     Create a constrained decimal using the constructor provided
--     Return Error if input is less than minVal or more than maxVal
--     let createDecimal fieldName ctor minVal maxVal i = 
--         if i < minVal then
--             let msg = sprintf "%s: Must not be less than %M" fieldName minVal
--             Error msg
--         elif i > maxVal then
--             let msg = sprintf "%s: Must not be greater than %M" fieldName maxVal
--             Error msg
--         else
--             Ok (ctor i)

--     Create a constrained string using the constructor provided
--     Return Error if input is null. empty, or does not match the regex pattern
createLike :: String -> (String -> a) -> String -> String -> Result a
createLike fieldName constructor pattern str
    | length str == 0
    = Error $ fieldName ++ " must not be empty"
    | not (matchRegex pattern str)
    = Error
        $  fieldName
        ++ " : '"
        ++ str
        ++ "' must match the pattern '"
        ++ pattern
        ++ "'"
    | otherwise
    = Ok $ constructor str
