module OrderTaking.Common.ConstrainedType
    ( createString
    , createStringOption
    )
where

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
--     let createInt fieldName ctor minVal maxVal i = 
--         if i < minVal then
--             let msg = sprintf "%s: Must not be less than %i" fieldName minVal
--             Error msg
--         elif i > maxVal then
--             let msg = sprintf "%s: Must not be greater than %i" fieldName maxVal
--             Error msg
--         else
--             Ok (ctor i)
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
--     let createLike fieldName  ctor pattern str = 
--         if String.IsNullOrEmpty(str) then
--             let msg = sprintf "%s: Must not be null or empty" fieldName 
--             Error msg
--         elif System.Text.RegularExpressions.Regex.IsMatch(str,pattern) then
--             Ok (ctor str)
--         else
--             let msg = sprintf "%s: '%s' must match the pattern '%s'" fieldName str pattern
--             Error msg 
