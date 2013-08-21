{-# LANGUAGE OverloadedStrings #-}

-- | The main Sparkle entry point.
--
-- (As the actress said to the bishop, of course.)
--

module Sparkle
    ( sparkle
    ) where

import qualified Data.Text as T
import Happstack.Server
import Web.Routes (RouteT, Site, nestURL, setDefault)
import Web.Routes.Boomerang (boomerangSiteRouteT)
import Web.Routes.Happstack (implSite)

import Sparkle.Common
import Sparkle.Handlers
import Sparkle.Routes
import qualified Sparkle.API as API

sparkle :: Text -> Int -> ServerPart Response
sparkle sHost sPort = mconcat
    [ implSite ("http://" <> sHost <> ":" <> T.pack (show sPort) <> "/") "" site
    , dir "static" $ serveDirectory DisableBrowsing [] "static"
    ]

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url = case url of
    Home -> homePage
    API url' -> nestURL API $ API.route url'

site :: Site Sitemap (ServerPart Response)
site = setDefault Home $ boomerangSiteRouteT route sitemap
