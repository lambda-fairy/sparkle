{-# LANGUAGE RankNTypes, OverloadedStrings, TemplateHaskell, TypeOperators #-}

module Sparkle.API.Routes where

import Prelude hiding ((.), id)
import Control.Category ((.), {-id-})
import Text.Boomerang.TH (makeBoomerangs)
import Web.Routes.Boomerang hiding ((<>), Pos)

import Sparkle.Common
import Sparkle.Types

data Sitemap
    = Project
    | Tasks Pos
    | TasksNew Pos
    deriving (Eq, Read, Show)
$(makeBoomerangs ''Sitemap)

sitemap :: Router () (Sitemap :- ())
sitemap
    =  rProject . "project"
    <> rTasksNew . ("tasks" </> rPos </> "new")
    <> rTasks . ("tasks" </> rPos)

rPos :: Router r (Pos :- r)
rPos = integer
