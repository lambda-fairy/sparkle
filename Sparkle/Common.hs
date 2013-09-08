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
import qualified Data.Aeson.TH as A
import Data.Char (toLower)
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Tree
import Data.Tree.Lens

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM a b = liftM concat $ mapM a b

stripTypeName :: String -> A.Options
stripTypeName prefix = A.defaultOptions{ A.fieldLabelModifier = processField }
  where
    processField
        = headToLower
        . fromMaybe (error $ "field name must be prefixed by " ++ prefix')
        . stripPrefix prefix'
    prefix' = '_' : prefix

headToLower :: String -> String
headToLower = _head %~ toLower
