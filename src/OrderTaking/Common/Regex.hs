{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Common.Regex
    ( matchRegex
    )
where

-- import Data.Text
import           Text.Regex.TDFA

matchRegex :: String -> String -> Bool
matchRegex pattern input = input =~ regex
  where
    regex :: String
    regex = pattern
