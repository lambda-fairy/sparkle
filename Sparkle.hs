{-# LANGUAGE OverloadedStrings #-}

-- | The main Sparkle entry point.
--
-- (As the actress said to the bishop, of course.)
--

module Sparkle
    ( sparkle
    , sparkleDebug
    ) where

import Control.Exception (bracket)
import Data.Acid (AcidState)
import Data.Acid.Local (openLocalState, createCheckpointAndClose)
import Data.Acid.Memory (openMemoryState)
import qualified Data.Text as T
import Happstack.Server
import Web.Routes (Site, mapRouteT, nestURL, setDefault)
import Web.Routes.Boomerang (boomerangSiteRouteT)
import Web.Routes.Happstack (implSite)

import Sparkle.Common
import Sparkle.Handlers
import Sparkle.Routes
import qualified Sparkle.API as API
import Sparkle.Types

-- | Start the Sparkle server.
sparkle
    :: Text   -- ^ Host name, used for URL rendering (e.g. @"sparkle.example.com"@)
    -> Int    -- ^ Port
    -> IO ()
sparkle sHost sPort
    = bracket (openLocalState emptyProject)
              (createCheckpointAndClose)
              (\acid -> simpleHTTP conf (sparkleServer acid sHost sPort))
  where
    conf = nullConf { port = sPort }

-- | Start the Sparkle server in debug mode.
--
-- * No data will be written to disk.
-- * The state is pre-loaded with a test project.
-- * The server binds to port 8000.
--
sparkleDebug :: IO ()
sparkleDebug = do
    acid <- openMemoryState testProject
    simpleHTTP nullConf (sparkleServer acid "localhost" (port nullConf))

sparkleServer :: AcidState Project -> Text -> Int -> ServerPart Response
sparkleServer acid sHost sPort = mconcat
    [ implSite ("http://" <> sHost <> ":" <> T.pack (show sPort) <> "/") "" (site acid)
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
