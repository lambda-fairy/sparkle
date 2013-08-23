{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Sparkle.API.Handlers where

import Data.Aeson
import qualified Data.ByteString.Lazy as L
import Happstack.Server
import Web.Routes (RouteT)
import Web.Routes.Happstack ()

import Sparkle.Common
import Sparkle.API.Routes
import Sparkle.Types

getBody :: ServerPart L.ByteString
getBody = do
    req  <- askRq
    reqBody <- liftIO $ takeRequestBody req
    return $ maybe "" unBody reqBody

toResponseJSON :: ToJSON a => a -> Response
toResponseJSON = toResponseBS "application/json" . encode

projectHandler :: RouteT Sitemap (ServerPartT IO) Response
projectHandler = ok . toResponseJSON $ testProject
