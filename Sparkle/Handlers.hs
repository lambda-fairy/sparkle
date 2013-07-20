{-# LANGUAGE OverloadedStrings #-}

module Sparkle.Handlers where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Happstack.Server (ServerPartT, Response, toResponse, ok)
import Web.Routes (RouteT)
import Web.Routes.Happstack ()

import Sparkle.Routes
import Sparkle.Templates
import Sparkle.Types

homePage :: RouteT Sitemap (ServerPartT IO) Response
homePage = ok . toResponse $
    pageTemplate $ Project
        { projTitle = "Ducks!"
        , projTasks =
            [ Task ">\")_" True
                [ Task "Duck" False []
                , Task "Duck" True []
                , Task "Goose!" False []
                ]
            , Task "\x2192 also utf-8 is c\x014D\x014Dl \x2190" False
                [ Task "<script>alert('Look at me! I fail at XSS!')</script>" False []
                ]
            ]
        }

helloPage :: Maybe Text -> RouteT Sitemap (ServerPartT IO) Response
helloPage name = ok . toResponse $ T.concat ["Hello, ", name', "!"]
  where
    name' = fromMaybe "stranger" name
