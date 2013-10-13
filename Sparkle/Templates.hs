{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Sparkle.Templates where

import Text.Blaze ((!), dataAttribute, toValue)
import Text.Blaze.Html5 (Html, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Sparkle.Common
import Sparkle.Types


pageTemplate :: Project -> Html
pageTemplate proj = outerTemplate (proj^.projTitle) content
  where
    content = H.section ! A.id "plan" ! A.class_ "plan" $
                planTemplate (proj^.projTasks)


outerTemplate :: Text -> Html -> Html
outerTemplate title body = H.docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "utf-8"
        H.title $ toHtml title
        H.link ! A.rel "stylesheet" ! A.href "/static/core.css"
        H.script ! A.src "/static/jquery-2.0.3.min.js" $ mempty
        H.script ! A.src "/static/undoer.js" $ mempty
        H.script ! A.src "/static/core.js" $ mempty
    H.body $ do
        H.h1 $ toHtml title
        body
        H.footer $ do
            H.p $ do
                "Powered by "
                H.a ! A.href "http://lfairy.github.io/sparkle" $ "Sparkle"


planTemplate :: Tasks -> Html
planTemplate tasks = H.ul $
    forM_ (zip [0 :: Integer ..] tasks) $ \(i, t) -> do
        H.li ! A.class_ ("task" <> onlyIf (t^.taskDone) " task-completed")
             ! dataAttribute "id" (toValue (show i)) $
            H.table . H.tr $ do
                H.td ! A.class_ "task-done" $ do
                    H.input ! A.type_ "checkbox"
                            ! A.disabled "disabled"
                            ! onlyIf (t^.taskDone) (A.checked "checked")
                    H.span $ mempty  -- for styling the checkbox
                H.td ! A.class_ "task-title"
                     ! A.tabindex "-1" $
                    toHtml (t^.taskTitle)


-- | @onlyif b x@ returns @x@ /only if/ @b@ is True.
onlyIf :: Monoid a => Bool -> a -> a
onlyIf False _ = mempty
onlyIf True  x = x
