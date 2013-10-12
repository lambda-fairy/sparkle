module Sparkle.API where

import Happstack.Server

import Sparkle.API.Handlers
import Sparkle.API.Routes

route :: Sitemap -> ApiM Response
route url = case url of
    Project -> projectHandler
    Tasks pos -> tasksHandler pos
    TasksNew pos -> tasksNewHandler pos
