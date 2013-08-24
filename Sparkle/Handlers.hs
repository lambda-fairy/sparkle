{-# LANGUAGE OverloadedStrings #-}

module Sparkle.Handlers where

import Data.Acid (AcidState)
import qualified Data.Text as T
import Happstack.Server (ServerPartT, Response, toResponse, ok)
import Web.Routes (RouteT)
import Web.Routes.Happstack ()

import Sparkle.Common
import Sparkle.Routes
import Sparkle.Templates
import Sparkle.Types

type SparkleM a = RouteT Sitemap (ReaderT (AcidState Project) (ServerPartT IO)) a

homePage :: SparkleM Response
homePage = ok . toResponse =<< pageTemplate <$> queryP QueryProject
