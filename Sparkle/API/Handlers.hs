{-# LANGUAGE FlexibleContexts, OverloadedStrings, TemplateHaskell #-}

module Sparkle.API.Handlers where

import Data.Acid (AcidState)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as A
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

tasksHandler :: NonEmpty Int -> ApiM Response
tasksHandler is = msum
    [ method GET >> tasksGet
    , method POST >> tasksPost
    , method DELETE >> tasksDelete
    ]
  where
    tasksGet = queryP (QueryTask is) >>= \r -> case r of
        Nothing -> notFound $ errorJSON "task cannot be found"
        Just task -> ok . toResponseJSON $ task
    -- TODO
    tasksPost = undefined
    tasksDelete = undefined


-- Miscellaneous utility functions -------------------------------------

getBody :: (ServerMonad m, MonadIO m) => m L.ByteString
getBody = do
    req  <- askRq
    reqBody <- liftIO $ takeRequestBody req
    return $ maybe "" unBody reqBody

getBodyJSON :: (Functor m, ServerMonad m, MonadIO m, FromJSON a) => m (Either String a)
getBodyJSON = A.eitherDecode' <$> getBody

withJSONBody
    :: (Functor m, FilterMonad Response m, ServerMonad m, MonadIO m, FromJSON a, ToJSON b)
    => (a -> m b) -> m Response
withJSONBody success = either failure success' =<< getBodyJSON
  where
    failure = badRequest . errorJSON
    success' = fmap toResponseJSON . success

toResponseJSON :: ToJSON a => a -> Response
toResponseJSON = toResponseBS "application/json" . A.encode

errorJSON :: String -> Response
errorJSON reason = toResponseJSON $ A.object ["reason" A..= reason]
