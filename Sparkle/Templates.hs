{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Sparkle.Templates where

import Control.Applicative
import Control.Monad
import Data.Monoid (Monoid(mempty))
import Data.Text (Text)
import Text.Blaze ((!))
import Text.Blaze.Html5 (Html, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Sparkle.Types


pageTemplate :: Project -> Html
pageTemplate = outerTemplate <$> projTitle <*> planTemplate . projTasks


outerTemplate :: Text -> Html -> Html
outerTemplate title body = H.docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "utf-8"
        H.title $ toHtml title
    H.body $ do
        H.h1 $ toHtml title
        H.div ! A.id "content" $ body
        H.footer $ do
            H.p $ do
                "Powered by "
                H.a ! A.href "http://lfairy.github.io/sparkle" $ "Sparkle"


planTemplate :: [Task] -> Html
planTemplate = (H.section ! A.id "plan") . dumpChildren
  where
    dumpChildren tasks =
        H.ul ! onlyIf (notNull tasks) (A.class_ "task-has-children") $
            forM_ tasks $ \Task{..} ->
                H.li $ do
                    -- Task title
                    H.p $ H.label $ do
                        H.input ! A.type_ "checkbox"
                                ! A.disabled "disabled"
                                ! onlyIf taskDone (A.checked "checked")
                        "\xA0" -- no-break space
                        H.span $ toHtml taskTitle

                    -- Recurse in child tasks
                    when (notNull taskChildren) $
                        dumpChildren taskChildren


-- | @onlyif b x@ returns @x@ /only if/ @b@ is True.
onlyIf :: Monoid a => Bool -> a -> a
onlyIf False _ = mempty
onlyIf True  x = x

-- | Determine if the given list is not empty. Synonym for @not . null@.
notNull :: [a] -> Bool
notNull = not . null
