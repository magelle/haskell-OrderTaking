module OrderTaking.Common.PdfAttachment
    ( PdfAttachment(..)
    )
where

import qualified Data.ByteString               as B

data PdfAttachment = MkPdfAttachment { name :: String, bytes :: B.ByteString} deriving (Eq, Show)
