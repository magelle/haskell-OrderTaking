module OrderTaking.Common.UsStateCode
    ( UsStateCode()
    , create
    , value
    )
where

import           OrderTaking.Common.Result
import           OrderTaking.Common.ConstrainedType

-- A zip code
data UsStateCode = MkUsStateCode String deriving (Eq, Show)

-- Return the string value inside a UsStateCode
value :: UsStateCode -> String
value (MkUsStateCode str) = str

-- Create a UsStateCode from a string
-- Return Left if input is null, empty, or doesn't have 2 letters
create :: String -> String -> Either ErrorMsg UsStateCode 
create fieldName = createLike fieldName MkUsStateCode "^(A[KLRZ]|C[AOT]|D[CE]|FL|GA|HI|I[ADLN]|K[SY]|LA|M[ADEINOST]|N[CDEHJMVY]|O[HKR]|P[AR]|RI|S[CD]|T[NX]|UT|V[AIT]|W[AIVY])$"
