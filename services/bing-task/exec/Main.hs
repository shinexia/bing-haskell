{- |
   Copyright  : Copyright (C) 2017 Shine Xia (https://xgh.io)
   License    : Apache License 2.0
   Maintainer : Shine Xia <shine.xgh@gmail.com>
-}

module Main (main) where

import Control.Monad (when)
import Bing.Task

main :: IO ()
main = do
    opts <- parseOpts
    print opts
    when (optCheckUnsaved opts) (runOpts opts runChecker)
    runOpts opts runWorker
