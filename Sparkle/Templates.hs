{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Sparkle.Templates where

import Data.Text (Text)
import Text.Blaze ((!))
import Text.Blaze.Html5 (Html, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

appTemplate :: Text -> Html -> Html
appTemplate title body = H.docTypeHtml $ do
    H.head $ do
        H.meta ! A.charset "utf-8"
        H.title $ do toHtml title; " - Sparkle"
    H.body $ do
        H.h1 $ toHtml title
        H.div ! A.id "content" $ body
        H.footer $ do
            H.p $ do
                "Powered by "
                H.a ! A.href "http://lfairy.github.io/sparkle" $ "Sparkle"
