module OrderTaking.Common.PdfAttachment
    ( PdfAttachment
    )
where

import qualified Data.ByteString               as B

data PdfAttachment = PdfAttachment { name :: String, bytes :: B.ByteString} deriving (Eq, Show)
