module Sparkle.Common
    ( module Control.Applicative
    , module Control.Monad
    , module Control.Monad.Reader
    , module Control.Monad.Trans
    , module Control.Lens
    , module Data.List
    , module Data.Maybe
    , module Data.Monoid
    , module Data.Tree
    , module Data.Tree.Lens
    , Text
    , concatMapM
    , stripTypeName
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Lens hiding (levels)
import Data.Char (toLower)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Tree
import Data.Tree.Lens

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM a b = liftM concat $ mapM a b

stripTypeName :: String -> String -> String
stripTypeName prefix field = headToLower field'
  where
    field' = fromMaybe (error $ "field name " ++ field ++ " must be prefixed by " ++ prefix')
                       (stripPrefix prefix' field)
    prefix' = '_' : prefix

headToLower :: String -> String
headToLower = _head %~ toLower
