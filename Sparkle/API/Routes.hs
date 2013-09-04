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
    <> rTasksNew . ("tasks" </> rNonEmptySep rInt eos </> "new")
    <> rTasks . ("tasks" </> rNonEmptySep rInt eos)

rNonEmptySep
    :: (forall r'. PrinterParser e tok r' (a :- r'))
    -> (forall r'. PrinterParser e tok r' r')
    -> PrinterParser e tok r (NonEmpty a :- r)
rNonEmptySep r sep = rNonEmpty . r . rList (sep . r)

rNonEmpty :: PrinterParser e tok (a :- [a] :- r) (NonEmpty a :- r)
rNonEmpty
    = xpure (\(x :- (xs :- r)) -> (x :| xs) :- r)
            (\((x :| xs) :- r) -> Just (x :- (xs :- r)))

-- | A non-broken version of 'integer'.
rInt :: PrinterParser TextsError [Text] r (Int :- r)
rInt = xmaph
    (\s -> case Text.decimal s of
                Left e -> error $ "rInt: " ++ e
                Right (x, s')
                  | Text.null s' -> x
                  | otherwise -> error "rInt: ambiguous parse")
    (Just . Text.pack . show)
    (rText1 digit)
