{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeOperators #-}

module Sparkle.API.Routes where

import Prelude hiding ((.), id)
import Control.Category ((.), {-id-})
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as L
import Text.Boomerang.TH (derivePrinterParsers)
import Web.Routes.Boomerang

data Sitemap
    = Project
    | Tasks (NonEmpty Integer)
    deriving (Eq, Read, Show)
$(derivePrinterParsers ''Sitemap)

sitemap :: Router () (Sitemap :- ())
sitemap
    =  rProject . "project"
    <> rTasks . ("tasks" </> rNonEmpty (integer . eos))

rNonEmpty :: PrinterParser e tok r (a :- r) -> PrinterParser e tok r (NonEmpty a :- r)
rNonEmpty r = xmaph L.fromList (Just . L.toList) (rList1 r)
