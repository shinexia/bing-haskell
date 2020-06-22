{-# LANGUAGE OverloadedStrings #-}

module Test where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)

import Bing.Core


p :: Page Integer
p = Page [1, 2, 3] True 100

s :: ByteString
s = "{\"total\":100,\"result\":[1,2,3],\"hasMore\":true}"

