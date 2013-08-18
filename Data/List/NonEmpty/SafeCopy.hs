{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module exports a 'SafeCopy' instance for 'NonEmpty'.

module Data.List.NonEmpty.SafeCopy () where

import Data.SafeCopy
import Data.List.NonEmpty (NonEmpty, nonEmpty, toList)

instance SafeCopy a => SafeCopy (NonEmpty a) where
    kind = primitive
    getCopy = contain $ do
        xs <- fmap nonEmpty safeGet
        maybe (fail "list is empty") return xs
    putCopy xs = contain $ safePut (toList xs)
