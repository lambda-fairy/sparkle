{-# LANGUAGE OverloadedStrings #-}

-- | The main Sparkle entry point.
--
-- (As the actress said to the bishop, of course.)
--

module Sparkle
    ( sparkle
    , sparkleDebug
    , sparkleWith
    ) where

import Control.Exception (bracket)
import Data.Acid (AcidState)
import Data.Acid.Local (openLocalState, createCheckpointAndClose)
import Data.Acid.Memory (openMemoryState)
import Happstack.Server
import Happstack.Server.Compression (compressedResponseFilter)
import Network.Simple.TCP
import Web.Routes (Site, mapRouteT, nestURL, setDefault)
import Web.Routes.Boomerang (boomerangSiteRouteT)
import Web.Routes.Happstack (implSite)

import Sparkle.Common
import Sparkle.Handlers
import Sparkle.Routes
import qualified Sparkle.API as API
import Sparkle.Types

-- | Start the Sparkle server.
sparkle :: HostPreference -> ServiceName -> IO ()
sparkle sHost sPort
    = bracket (openLocalState emptyProject)
              (createCheckpointAndClose)
              (sparkleWith sHost sPort)

-- | Start the Sparkle server in debug mode.
--
-- * No data will be written to disk.
-- * The state is pre-loaded with a test project.
-- * The server binds to localhost, port 8000.
--
sparkleDebug :: IO ()
sparkleDebug = do
    acid <- openMemoryState testProject
    sparkleWith "localhost" "8000" acid

sparkleWith :: HostPreference -> ServiceName -> AcidState Project -> IO ()
sparkleWith sHost sPort acid =
    -- Bind our own socket rather than depending on Happstack
    -- network-simple enables TCP_NODELAY by default, which reduces latency
    -- TODO: do something with addr
    listen sHost sPort $ \(sock, _addr) ->
        simpleHTTPWithSocket sock nullConf (sparkleSP acid)

sparkleSP :: AcidState Project -> ServerPart Response
sparkleSP acid = compressedResponseFilter >> mconcat
    [ implSite "/" "" (site acid)
    , dir "static" $ serveDirectory DisableBrowsing [] "static"
    ]

site :: AcidState Project -> Site Sitemap (ServerPartT IO Response)
site acid = setDefault Home $ boomerangSiteRouteT route' sitemap
  where
    route' = mapRouteT (flip runReaderT acid) . route

route :: Sitemap -> SparkleM Response
route url = fancyHeaders <$> case url of
    Home -> homePage
    API url' -> nestURL API $ API.route url'
  where
    fancyHeaders = setHeader "X-UA-Compatible" "IE=edge"
