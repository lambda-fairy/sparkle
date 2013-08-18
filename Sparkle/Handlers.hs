{-# LANGUAGE OverloadedStrings #-}

module Sparkle.Handlers where

import qualified Data.Text as T
import Happstack.Server (ServerPartT, Response, toResponse, ok)
import Web.Routes (RouteT)
import Web.Routes.Happstack ()

import Sparkle.Common
import Sparkle.Routes
import Sparkle.Templates
import Sparkle.Types

homePage :: RouteT Sitemap (ServerPartT IO) Response
homePage = ok . toResponse $
    pageTemplate $ emptyProject
        & projTitle .~ "Ducks!"
        & projTasks .~ testTasks

helloPage :: Maybe Text -> RouteT Sitemap (ServerPartT IO) Response
helloPage name = ok . toResponse $ T.concat ["Hello, ", name', "!"]
  where
    name' = fromMaybe "stranger" name
