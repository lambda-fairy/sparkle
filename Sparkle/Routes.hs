{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeOperators #-}

module Sparkle.Routes
    ( Sitemap(..)
    , sitemap
    ) where

import Prelude hiding ((.), id)
import Control.Category ((.), {-id-})
import Text.Boomerang.TH (makeBoomerangs)
import Web.Routes.Boomerang

import qualified Sparkle.API.Routes as API

data Sitemap = Home | API API.Sitemap
    deriving (Eq, Read, Show)
$(makeBoomerangs ''Sitemap)

sitemap :: Router () (Sitemap :- ())
sitemap
    =  rHome
    <> rAPI . ("api" </> "v0" </> API.sitemap)
