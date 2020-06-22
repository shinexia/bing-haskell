{- |
   Copyright  : Copyright (C) 2017 Shine Xia (https://www.xgh.io)
   License    : Apache License 2.0
   Maintainer : Shine Xia <ishine.xia@gmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}

module Bing.Data.Types where

import Prelude ()
import Prelude.Compat

import Data.Aeson
import Data.Map (Map)
import qualified Data.Text.Lazy as TL
import Bing.Core.JsonUtil ((#))

type TextType = TL.Text

type Config = Map String String

type PictDate          = Integer
type PictUrl           = TextType
type PictOriginUrl     = PictUrl
type PictSavedPath     = PictUrl
type PictCopyright     = TextType
type PictCopyrightlink = TextType

-- 数据库对象
data Pict = Pict {
    pictDate          :: PictDate           -- 日期，格式：20170901
  , pictOriginUrl     :: PictOriginUrl      -- 源地址，加上`https://www.bing.com`即可下载
  , pictSavedPath     :: PictSavedPath      -- 下载到本地的路径
  , pictCopyright     :: PictCopyright      -- 描述
  , pictCopyrightlink :: PictCopyrightlink  -- 搜索
} deriving (Eq, Show)

-- 序列化
instance ToJSON Pict where
    toJSON p = object
        $ "date"           .= pictDate p
        # "origin_url"     .= pictOriginUrl p
        # "saved_path"     .= pictSavedPath p
        # "copyright"      .= pictCopyright p
        # "copyrightlink"  .= pictCopyrightlink p
        # []

-- 解析
instance FromJSON Pict where
    parseJSON = withObject "pict" $ \o ->
        Pict <$> o .:  "date"
             <*> o .:? "origin_url"    .!= ""
             <*> o .:? "saved_path"    .!= ""
             <*> o .:? "copyright"     .!= ""
             <*> o .:? "copyrightlink" .!= ""
