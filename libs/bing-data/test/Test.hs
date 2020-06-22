{- |
   Copyright  : Copyright (C) 2017 Shine Xia (https://www.xgh.io)
   License    : Apache License 2.0
   Maintainer : Shine Xia <ishine.xia@gmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}
module Test
    (
    ) where

import Bing.Data.Types
import Data.Aeson
import Data.ByteString.Lazy (ByteString)

p = Pict 20170905 "url_test" "path_test" "copyright_test" "copyrightlink_test":: Pict

s = "{\"copyright\":\"copyright_test\",\"url\":\"url_test\",\"date\":\"20170905\"}" :: ByteString
