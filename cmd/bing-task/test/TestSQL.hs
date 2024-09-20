{- |
   Copyright  : Copyright (C) 2017 Shine Xia (https://www.xgh.io)
   License    : Apache License 2.0
   Maintainer : Shine Xia <ishine.xia@gmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bing.Task

main :: IO ()
main = do
    let opts = Opts "test.conf" ".tmp" False
    runOpts opts runChecker
