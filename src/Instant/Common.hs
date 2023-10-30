{-# LANGUAGE FlexibleInstances #-}

module Instant.Common where

import Data.Text.Lazy.Builder (Builder)

withIndent :: String -> String
withIndent s = "  " ++ s

class Emit x where
    emit :: x -> Builder