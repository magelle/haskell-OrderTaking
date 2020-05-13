module OrderTaking.Common.ConstrainedType
    ( createString
    , createStringOption
    , createLike
    , createInt
    , createDecimal
    )
where

import           OrderTaking.Common.Regex
import           OrderTaking.Common.Result

-- ===============================
-- Reusable constructors and getters for constrained types
-- ===============================
--     Create a constrained string using the constructor provided
--     Return Left if input is null, empty, or length > maxLen
createString :: String -> (String -> a) -> Int -> String -> Either ErrorMsg a
createString fieldName constructor maxLen str
    | length str == 0
    = Left $ fieldName ++ " must not be empty"
    | length str > maxLen
    = Left
        $  fieldName
        ++ " must not be more than "
        ++ (show maxLen)
        ++ " chars"
    | otherwise
    = Right $ constructor str

--     Create a optional constrained string using the constructor provided
--     Return None if input is null, empty. 
--     Return error if length > maxLen
--     Return Some if the input is valid
createStringOption
    :: String -> (String -> a) -> Int -> String -> Either ErrorMsg (Maybe a)
createStringOption fieldName constructor maxLen str
    | length str == 0
    = Right Nothing
    | length str > maxLen
    = Left
        $  fieldName
        ++ " must not be more than "
        ++ (show maxLen)
        ++ " chars"
    | otherwise
    = Right (Just $ constructor str)

--     Create a constrained integer using the constructor provided
--     Return Left if input is less than minVal or more than maxVal
createInt :: String -> (Int -> a) -> Int -> Int -> Int -> Either ErrorMsg a
createInt fieldName constructor minVal maxVal i
    | i < minVal
    = Left $ fieldName ++ ": Must not be less than " ++ (show minVal)
    | i > maxVal
    = Left $ fieldName ++ ": Must not be greater than " ++ (show maxVal)
    | otherwise
    = Right (constructor i)

--     Create a constrained decimal using the constructor provided
--     Return Left if input is less than minVal or more than maxVal
createDecimal
    :: String -> (Double -> a) -> Double -> Double -> Double -> Either ErrorMsg a
createDecimal fieldName constructor minVal maxVal i
    | i < minVal
    = Left $ fieldName ++ ": Must not be less than " ++ (show minVal)
    | i > maxVal
    = Left $ fieldName ++ ": Must not be greater than " ++ (show maxVal)
    | otherwise
    = Right (constructor i)

--     Create a constrained string using the constructor provided
--     Return Left if input is null. empty, or does not match the regex pattern
createLike :: String -> (String -> a) -> String -> String -> Either ErrorMsg a
createLike fieldName constructor pattern str
    | length str == 0
    = Left $ fieldName ++ " must not be empty"
    | not (matchRegex pattern str)
    = Left
        $  fieldName
        ++ " : '"
        ++ str
        ++ "' must match the pattern '"
        ++ pattern
        ++ "'"
    | otherwise
    = Right $ constructor str
