{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Sparkle.Routes
    ( Sitemap(..)
    , sitemap
    ) where

import Prelude hiding ((.), id)
import Control.Category ((.), id)
import Data.Text (Text)
import Text.Boomerang.TH (derivePrinterParsers)
import Web.Routes.Boomerang

data Sitemap = Home | Hello (Maybe Text)
    deriving (Eq, Ord, Read, Show)
$(derivePrinterParsers ''Sitemap)

sitemap :: Router () (Sitemap :- ())
sitemap =
    (  rHome
    <> rHello . (lit "hello" </> rMaybe anyText)
    )
