{-# LANGUAGE RankNTypes, OverloadedStrings, TemplateHaskell, TypeOperators #-}

module Sparkle.API.Routes where

import Prelude hiding ((.), id)
import Control.Category ((.), {-id-})
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import Text.Boomerang.TH (derivePrinterParsers)
import Web.Routes.Boomerang hiding ((<>), Pos)

import Sparkle.Common
import Sparkle.Types

data Sitemap
    = Project
    | Tasks Pos
    | TasksNew Pos
    deriving (Eq, Read, Show)
$(derivePrinterParsers ''Sitemap)

sitemap :: Router () (Sitemap :- ())
sitemap
    =  rProject . "project"
    <> rTasksNew . ("tasks" </> rNonEmptySep int eos </> "new")
    <> rTasks . ("tasks" </> rNonEmptySep int eos)

rNonEmptySep
    :: (forall r'. PrinterParser e tok r' (a :- r'))
    -> (forall r'. PrinterParser e tok r' r')
    -> PrinterParser e tok r (NonEmpty a :- r)
rNonEmptySep r sep = rNonEmpty . r . rList (sep . r)

rNonEmpty :: PrinterParser e tok (a :- [a] :- r) (NonEmpty a :- r)
rNonEmpty
    = xpure (\(x :- (xs :- r)) -> (x :| xs) :- r)
            (\((x :| xs) :- r) -> Just (x :- (xs :- r)))
