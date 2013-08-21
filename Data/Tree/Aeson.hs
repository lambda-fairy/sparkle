{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- 'FromJSON' and 'ToJson' instances for 'Tree'.

module Data.Tree.Aeson () where

import Control.Applicative
import Data.Aeson
import Data.Tree

instance FromJSON a => FromJSON (Tree a) where
    parseJSON (Object v)
        = Node <$> v .: "data" <*> v .: "children"
    parseJSON _ = empty

instance ToJSON a => ToJSON (Tree a) where
    toJSON (Node data_ children)
        = object ["data" .= data_, "children" .= children]
