{-# LANGUAGE FlexibleContexts, OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Sparkle.API.Handlers where

import Data.Acid (AcidState)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as L
import Happstack.Server
import Web.Routes (RouteT)
import Web.Routes.Happstack ()

import Sparkle.Common
import Sparkle.API.Routes
import Sparkle.Types

type ApiM a = RouteT Sitemap (ReaderT (AcidState Project) (ServerPartT IO)) a

projectHandler :: ApiM Response
projectHandler = ok . render =<< queryP QueryProject

tasksHandler :: Pos -> ApiM Response
tasksHandler _ = error "not implemented"

tasksDataHandler :: Pos -> ApiM Response
tasksDataHandler is = msum
    [ method GET >> doGet
    , method POST >> doPost
    , method DELETE >> doDelete
    ]
  where
    doGet = queryP (QueryTask is) >>= \r -> case r of
        Nothing -> notFound $ reply "task cannot be found"
        Just task -> ok $ render task
    doPost = withJSONBody $ \task -> do
        updateP (ReplaceTask is task)
        noContent $ reply "task changed"
    doDelete = do
        updateP (DeleteTask is)
        noContent $ reply "task deleted"

tasksNewHandler :: Pos -> ApiM Response
tasksNewHandler is = do
    method POST
    withJSONBody $ \task -> do
        updateP (InsertTask is task)
        resp 201 {- Created -} $ reply "task added"


-- Miscellaneous utility functions -------------------------------------

getBody :: (ServerMonad m, MonadIO m) => m L.ByteString
getBody = do
    req  <- askRq
    reqBody <- liftIO $ takeRequestBody req
    return $ maybe "" unBody reqBody

getBodyJSON :: (Functor m, ServerMonad m, MonadIO m, FromJSON a) => m (Either String a)
getBodyJSON = A.eitherDecode' <$> getBody

withJSONBody
    :: (Functor m, FilterMonad Response m, ServerMonad m, MonadIO m, FromJSON a)
    => (a -> m Response) -> m Response
withJSONBody success = either failure success =<< getBodyJSON
  where
    failure = badRequest . reply

render :: ToJSON a => a -> Response
render = toResponseBS "application/json" . A.encode

reply :: String -> Response
reply message = render $ A.object ["message" A..= message]
