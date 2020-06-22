{- |
   Copyright  : Copyright (C) 2017 Shine Xia (https://www.xgh.io)
   License    : Apache License 2.0
   Maintainer : Shine Xia <ishine.xia@gmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Bing.Core.Types
    ( Page (..)
    , emptyPage
    ) where

import Data.Int (Int64)
import Data.Aeson
import Bing.Core.JsonUtil ((#))

data Page a = Page
    { resultList     :: [a]
    , resultHasMore  :: !Bool
    , totalCount     :: !Int64
    } deriving (Functor, Eq, Show)

-- | A page with an empty result list.
emptyPage :: Page a
emptyPage = Page [] False 0

instance (ToJSON a) => ToJSON (Page a) where
    toJSON p = object
        $ "result"  .= resultList p
        # "hasMore" .= resultHasMore p
        # "total"   .= totalCount p
        # []

instance (FromJSON a) => FromJSON (Page a) where
    parseJSON = withObject "page" $ \o -> do
        r <- o .: "result"
        h <- o .: "hasMore"
        c <- o .: "total"
        return (Page r h c)
