{- |
   Copyright  : Copyright (C) 2017 Shine Xia (https://xgh.io)
   License    : Apache License 2.0
   Maintainer : Shine Xia <shine.xgh@gmail.com>
-}

module Bing.Task.Util
    ( readDate
    , imageToPict
    , getFileName
    , generateSavePath
    , download
    ) where

import Network.HTTP.Simple
import Text.Regex.PCRE
import Text.Printf
import Data.Text.Lazy.Read
import Data.Text.Lazy (Text)
import Bing.Data.Types
import Bing.Bot.Types
import System.Directory

import qualified Data.ByteString.Lazy as LB

readDate :: Text -> PictDate
readDate text =
    case decimal text of
        (Right (x, _)) -> x
        _              -> 0


-- @param1 保存路径（子路径）
-- @param2 Image
-- @return Pict
imageToPict :: PictSavedPath -> Image -> Pict
imageToPict s a = Pict {
    pictDate          = readDate $ _enddate a
  , pictOriginUrl     = _url a
  , pictSavedPath     = s
  , pictCopyright     = _copyright a
  , pictCopyrightlink = _copyrightlink a
}


-- @param1 图片的下载地址 （子路径）
-- @return 图片的filename
getFileName :: String -> String
getFileName url = do
    let matches = url =~ "[^/]+$" :: AllTextMatches [] String
    let results = getAllTextMatches matches :: [String]
    case results of
        [x] -> x
        _   -> ""

-- @param1 图片的原始下载地址（子路径）
-- @param2 图片的日期
-- @return 图片的本地保存路径（子路径）
generateSavePath :: PictDate -> String
generateSavePath date = let
        year      = date `div` 10000
        month     = (date `div` 100) - (year * 100)
        day       = date `mod` 100
        in "/" ++ show year ++            -- /:year
           "/" ++ printf "%02d" month ++  -- /:month
           "/" ++ printf "%02d" day       -- /:day

-- @param1 下载地址（全路径）
-- @param2 保存地址（全路径）
download :: String -> String -> String -> IO ()
download url dir filename = do
    req <- parseRequest url
    response <- httpLBS req
    let file = getResponseBody response
    -- putStrLn $ "begin to save file to " ++ save
    createDirectoryIfMissing True dir
    LB.writeFile (dir ++ "/" ++ filename) file
