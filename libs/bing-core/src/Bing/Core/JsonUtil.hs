{- |
   Copyright  : Copyright (C) 2017 Shine Xia (https://www.xgh.io)
   License    : Apache License 2.0
   Maintainer : Shine Xia <ishine.xia@gmail.com>
-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Bing.Core.JsonUtil
    ( append
    , (#)
    ) where

import Data.Aeson
import Data.Aeson.Types

append :: Pair -> [Pair] -> [Pair]
append (_, Null) pp = pp
append p         pp = p:pp
{-# INLINE append #-}

infixr 5 #

(#) :: Pair -> [Pair] -> [Pair]
(#) = append
{-# INLINE (#) #-}
