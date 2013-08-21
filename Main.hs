{-# LANGUAGE OverloadedStrings #-}

module Main where

import Happstack.Server

import Sparkle

main :: IO ()
main = simpleHTTP conf (sparkle "localhost" (port conf))
  where conf = nullConf
