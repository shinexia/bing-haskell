{- |
   Copyright  : Copyright (C) 2017 Shine Xia (https://www.xgh.io)
   License    : Apache License 2.0
   Maintainer : Shine Xia <ishine.xia@gmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}

module Bing.Bot.Types where

import qualified Data.Text.Lazy as L
import Data.Aeson hiding (Result)
import Bing.Core.JsonUtil ((#))

type TextType = L.Text

data Result = Result {
    _images  :: [Image],
    _tooltips :: Tooltip
} deriving (Show)

-- 序列化
instance ToJSON Result where
    toJSON p = object
        $ "images"     .= _images p
        # "tooltips"   .= _tooltips p
        # []

-- 解析
instance FromJSON Result where
    parseJSON = withObject "result" $ \o ->
        Result <$> o .:? "images"    .!= []
               <*> o .:? "tooltips"  .!= emptyTooltip

data Image = Image {
    _startdate     :: TextType,
    _fullstartdate :: TextType,
    _enddate       :: TextType,
    _url           :: TextType,
    _urlbase       :: TextType,
    _copyright     :: TextType,
    _copyrightlink :: TextType,
    _quiz          :: TextType,
    _wp            :: Bool,
    _hsh           :: TextType,
    _drk           :: Int,
    _top           :: Int,
    _bot           :: Int,
    _hs            :: [TextType]
} deriving (Show)

instance ToJSON Image where
    toJSON p = object
        $ "startdate"      .= _startdate p
        # "fullstartdate"  .= _fullstartdate p
        # "enddate"        .= _enddate p
        # "url"            .= _url p
        # "urlbase"        .= _urlbase p
        # "copyright"      .= _copyright p
        # "copyrightlink"  .= _copyrightlink p
        # "quiz"           .= _quiz p
        # "wp"             .= _wp p
        # "hsh"            .= _hsh p
        # "drk"            .= _drk p
        # "top"            .= _top p
        # "bot"            .= _bot p
        # "hs"             .= _hs p
        # []

-- 解析
instance FromJSON Image where
    parseJSON = withObject "image" $ \o ->
        Image <$> o .:? "startdate"      .!= ""
              <*> o .:? "fullstartdate"  .!= ""
              <*> o .:? "enddate"        .!= ""
              <*> o .:? "url"            .!= ""
              <*> o .:? "urlbase"        .!= ""
              <*> o .:? "copyright"      .!= ""
              <*> o .:? "copyrightlink"  .!= ""
              <*> o .:? "quiz"           .!= ""
              <*> o .:? "wp"             .!= False
              <*> o .:? "hsh"            .!= ""
              <*> o .:? "drk"            .!= 0
              <*> o .:? "top"            .!= 0
              <*> o .:? "bot"            .!= 0
              <*> o .:? "hs"             .!= []


data Tooltip = Tooltip {
    _loading       :: TextType,
    _previous      :: TextType,
    _next          :: TextType,
    _walle         :: TextType,
    _walls         :: TextType,
    _play          :: TextType,
    _pause         :: TextType
} deriving (Show)

emptyTooltip :: Tooltip
emptyTooltip = Tooltip {
    _loading  = ""
  , _previous = ""
  , _next     = ""
  , _walle    = ""
  , _walls    = ""
  , _play     = ""
  , _pause    = ""
}

-- 序列化
instance ToJSON Tooltip where
    toJSON p = object
        $ "loading"    .= _loading p
        # "previous"   .= _previous p
        # "next"       .= _next p
        # "walle"      .= _walle p
        # "walls"      .= _walls p
        # "play"       .= _play p
        # "pause"      .= _play p
        # []

-- 解析
instance FromJSON Tooltip where
    parseJSON = withObject "pooltip" $ \o ->
        Tooltip <$> o .:? "loading"   .!= ""
                <*> o .:? "previous"  .!= ""
                <*> o .:? "next"      .!= ""
                <*> o .:? "walle"     .!= ""
                <*> o .:? "walls"     .!= ""
                <*> o .:? "play"      .!= ""
                <*> o .:? "pause"     .!= ""
