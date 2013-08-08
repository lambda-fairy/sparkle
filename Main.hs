{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid (mconcat)
import Happstack.Server
import Web.Routes (RouteT, Site, setDefault)
import Web.Routes.Boomerang (boomerangSiteRouteT)
import Web.Routes.Happstack (implSite)

import Sparkle.Handlers
import Sparkle.Routes

main :: IO ()
main = simpleHTTP nullConf $ mconcat
    [ implSite "http://localhost:8000" "" site
    , dir "static" $ serveDirectory DisableBrowsing [] "static"
    ]

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url = case url of
    Home -> homePage
    Hello name -> helloPage name

site :: Site Sitemap (ServerPartT IO Response)
site = setDefault Home $ boomerangSiteRouteT route sitemap
