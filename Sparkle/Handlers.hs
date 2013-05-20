{-# LANGUAGE OverloadedStrings #-}

module Sparkle.Handlers where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Happstack.Server (ServerPartT, Response, toResponse, ok)
import Web.Routes (RouteT)
import Web.Routes.Happstack ()

import Sparkle.Routes

homePage :: RouteT Sitemap (ServerPartT IO) Response
homePage = ok . toResponse $ T.pack "This is the home page!"

helloPage :: Maybe Text -> RouteT Sitemap (ServerPartT IO) Response
helloPage name = ok . toResponse $ T.concat ["Hello, ", name', "!"]
  where
    name' = fromMaybe "stranger" name
