{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Sparkle.API.Handlers where

import Data.Acid (AcidState)
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import Data.List.NonEmpty (NonEmpty)
import Happstack.Server
import Web.Routes (RouteT)
import Web.Routes.Happstack ()

import Sparkle.Common
import Sparkle.API.Routes
import Sparkle.Types

type ApiM a = RouteT Sitemap (ReaderT (AcidState Project) (ServerPartT IO)) a

projectHandler :: ApiM Response
projectHandler = ok . toResponseJSON =<< queryP QueryProject

tasksHandler :: NonEmpty Integer -> ApiM Response
tasksHandler is = msum
    [ method GET >> tasksGet
    , method POST >> tasksPost
    , method DELETE >> tasksDelete
    ]
  where
    -- TODO
    tasksGet = undefined
    tasksPost = undefined
    tasksDelete = undefined

getBody :: (ServerMonad m, MonadIO m) => m L.ByteString
getBody = do
    req  <- askRq
    reqBody <- liftIO $ takeRequestBody req
    return $ maybe "" unBody reqBody

getBodyJSON :: (ServerMonad m, MonadIO m, MonadPlus m, FromJSON a) => m a
getBodyJSON = getBody >>= maybe mzero return . decode

toResponseJSON :: ToJSON a => a -> Response
toResponseJSON = toResponseBS "application/json" . encode
