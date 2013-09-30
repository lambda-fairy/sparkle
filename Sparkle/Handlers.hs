{-# LANGUAGE OverloadedStrings #-}

module Sparkle.Handlers where

import Data.Acid (AcidState)
import Happstack.Server
import Web.Routes (RouteT)
import Web.Routes.Happstack ()

import Sparkle.Common
import Sparkle.Routes
import Sparkle.Templates
import Sparkle.Types

type SparkleM a = RouteT Sitemap (ReaderT (AcidState Project) (ServerPartT IO)) a

homePage :: SparkleM Response
homePage = do
    proj <- queryP QueryProject
    plain <- queryString (lookText' "plain") <|> pure ""
    let render
          | plain == "true" = planTemplate . view projTasks
          | otherwise = pageTemplate
    ok . toResponse $ render proj
