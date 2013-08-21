module Sparkle.API where

import Happstack.Server
import Web.Routes (RouteT)

import Sparkle.API.Handlers
import Sparkle.API.Routes

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url = case url of
    Project -> projectHandler
    Tasks pos -> undefined
