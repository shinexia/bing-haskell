{-# LANGUAGE OverloadedStrings #-}

module Bing.Rest.Types
    ( PictDTO(PictDTO, pictDTODate, pictDTOUrl, pictDTOCopyright, pictDTOCopyrightlink)
    , makeDTO
    , ResultPage(ResultPage, resultPageRecords, resultPageCount, resultPageHasMore)
    ) where

import Prelude ()
import Prelude.Compat
import Data.Monoid ((<>))

import Data.Aeson
import Data.Int (Int32)
import Bing.Core.JsonUtil ((#))
import Bing.Data.Types


-- 返回给Webapp的对象
data PictDTO = PictDTO {
   pictDTODate             :: PictDate  -- 日期，格式：20170901
   , pictDTOUrl            :: TextType   -- 地址
   , pictDTOCopyright      :: PictCopyright  -- 描述
   , pictDTOCopyrightlink  :: PictCopyrightlink  -- 搜索
} deriving (Eq, Show)


-- 合并两个url，如果第二个为空，则返回空
makeURL :: TextType -> TextType -> TextType
makeURL a b
    | b == "" = ""
    | otherwise = a <> b


-- 创建DTO
makeDTO :: Pict -> PictDTO
makeDTO a = PictDTO
    (pictDate a)
    (makeURL "//static.xgh.io" (pictSavedPath a))
    (pictCopyright a)
    (makeURL "https://www.bing.com" (pictCopyrightlink a))


-- 序列化
instance ToJSON PictDTO where
   toJSON p = object
       $ "date"           .= pictDTODate p
       # "url"            .= pictDTOUrl p
       # "copyright"      .= pictDTOCopyright p
       # "copyrightlink"  .= pictDTOCopyrightlink p
       # []


-- 解析
instance FromJSON PictDTO where
   parseJSON = withObject "pictDTO" $ \o ->
       PictDTO <$> o  .:   "date"
               <*> o  .:?  "url"            .!= ""
               <*> o  .:?  "copyright"      .!= ""
               <*> o  .:?  "copyrightlink"  .!= ""


-- 分页对象
data ResultPage a = ResultPage {
    resultPageRecords :: [a]
  , resultPageCount   :: Int32
  , resultPageHasMore :: Bool
} deriving (Eq, Show)

-- instance ToJSON (ResultPage a) where
--     toJSON p = object
--         $ "records"   .= resultPageRecords p
--         # "count"     .= resultPageCount p
--         # "hasMore"   .= resultPageHasMore p
--         # []
--
-- instance FromJSON (ResultPage a) where
--     parseJSON = withObject "resultPage" $ \o ->
--         ResultPage <$>  o  .:?  "records" .!= []
--                    <*>  o  .:?  "count"   .!= 0
--                    <*>  o  .:?  "hasMore" .!= False
