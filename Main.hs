{-# LANGUAGE OverloadedStrings #-}

module Main where

import Happstack.Server (ServerPartT, Response, nullConf, simpleHTTP)
import Web.Routes (RouteT, Site, setDefault)
import Web.Routes.Boomerang (boomerangSiteRouteT)
import Web.Routes.Happstack (implSite)

import Sparkle.Handlers
import Sparkle.Routes

main :: IO ()
main = simpleHTTP nullConf $ implSite "http://localhost:8000" "" site

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url = case url of
    Home -> homePage
    Hello name -> helloPage name

site :: Site Sitemap (ServerPartT IO Response)
site = setDefault Home $ boomerangSiteRouteT route sitemap
