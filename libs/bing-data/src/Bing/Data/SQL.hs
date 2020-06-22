{- |
   Copyright  : Copyright (C) 2017 Shine Xia (https://www.xgh.io)
   License    : Apache License 2.0
   Maintainer : Shine Xia <ishine.xia@gmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}

module Bing.Data.SQL
    ( insertPict
    , selectOnePictByDate
    , selectPictListOfEmptySavedPath
    , updatePictSavedPath
    , selectPictListBetween
    , selectPictListByMonth
    , selectLatestDate
    )where

import Data.String (fromString)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.HDBC
import Bing.Data.Types
import Bing.Data.Util

-- 需要查询的所有表字段
basePictColumnList :: String
basePictColumnList = "date, origin_url, saved_path, copyright, copyrightlink"

-- fromSql
decodePictFromSql :: [SqlValue] -> Maybe Pict
decodePictFromSql [date, originUrl, savedPath, copyright, copyrightlink] =
    Just $ Pict date' originUrl' savedPath' copyright' copyrightlink'
    where date'          = fromSql date          :: PictDate
          originUrl'     = fromSql originUrl     :: PictOriginUrl
          savedPath'     = fromSql savedPath     :: PictSavedPath
          copyright'     = fromSql copyright     :: PictCopyright
          copyrightlink' = fromSql copyrightlink :: PictCopyrightlink
decodePictFromSql _ = Nothing

-- toSql
encodePictToSql :: Pict -> [SqlValue]
encodePictToSql p = [toSql $ pictDate p
                   , toSql $ pictOriginUrl p
                   , toSql $ pictSavedPath p
                   , toSql $ encodeUtf8 (pictCopyright p)
                   , toSql $ pictCopyrightlink p]

-- 创建一条新的记录
insertPict :: (IConnection conn) => conn -> Pict -> IO Integer
insertPict = runExecute (compose sql encodePictToSql)
    where
        sql :: PrepQuery Pict Integer
        sql = fromString $ "INSERT INTO bing_pict"
              ++ " (" ++ basePictColumnList ++ ")"
              ++ " VALUES (?, ?, ?, ?, ?)"

-- 查询某天的信息
selectOnePictByDate :: (IConnection conn) => conn -> PictDate -> IO (Maybe Pict)
selectOnePictByDate = runSelect (compose sql encode) decode
    where
        sql :: PrepQuery PictDate (Maybe Pict)
        sql = fromString $ "SELECT " ++ basePictColumnList ++ " FROM bing_pict"
            ++ " WHERE date = ?"

        encode :: PictDate -> [SqlValue]
        encode date = [toSql date]

        decode :: [[SqlValue]] -> Maybe Pict
        decode = decodeOne decodePictFromSql

-- 查询saved_path字段为空的记录
selectPictListOfEmptySavedPath :: IConnection conn => conn -> () -> IO [Pict]
selectPictListOfEmptySavedPath = runSelect (compose sql encodeEmpty) (decodeList decodePictFromSql)
    where
        sql :: PrepQuery () [Pict]
        sql = fromString $ "SELECT " ++ basePictColumnList ++ " FROM bing_pict"
            ++ " WHERE saved_path = ''"

-- 修改保存路径
updatePictSavedPath :: (IConnection conn) => conn -> (PictDate, PictSavedPath) -> IO Integer
updatePictSavedPath = runExecute (compose sql encode)
    where
        sql :: PrepQuery (PictDate, PictSavedPath) Integer
        sql = fromString $ "UPDATE bing_pict SET saved_path = ? "
            ++ " WHERE date = ?"

        encode :: (PictDate, PictSavedPath) -> [SqlValue]
        encode (date, path) = [toSql path, toSql date]

-- 分页查询
selectPictListBetween :: (IConnection conn) => conn
                                            -> (Maybe PictDate, Maybe PictDate, Offset, Limit) -> IO [Pict]
selectPictListBetween = runSelect encode decode
    where
        encode :: (Maybe PictDate, Maybe PictDate, Offset, Limit)
               -> (PrepQuery (Maybe PictDate, Maybe PictDate, Offset, Limit) [Pict], [SqlValue])
        encode (Just start, Just end, offset, limit) = (sql, [toSql start, toSql end, toSql offset, toSql limit])
            where
                sql :: PrepQuery (Maybe PictDate, Maybe PictDate, Offset, Limit) [Pict]
                sql = fromString $ "SELECT " ++ basePictColumnList ++ " FROM bing_pict"
                    ++ " WHERE date >= ? AND date <= ? LIMIT ?, ?"
        encode (Just start, Nothing, offset, limit) = (sql, [toSql start, toSql offset, toSql limit])
            where
                sql :: PrepQuery (Maybe PictDate, Maybe PictDate, Offset, Limit) [Pict]
                sql = fromString $ "SELECT " ++ basePictColumnList ++ " FROM bing_pict"
                    ++ " WHERE date >= ? LIMIT ?, ?"
        encode (Nothing, Just end, offset, limit) = (sql, [toSql end, toSql offset, toSql limit])
            where
                sql :: PrepQuery (Maybe PictDate, Maybe PictDate, Offset, Limit) [Pict]
                sql = fromString $ "SELECT " ++ basePictColumnList ++ " FROM bing_pict"
                    ++ " WHERE date <= ? LIMIT ?, ?"
        encode (Nothing, Nothing, offset, limit) = (sql, [toSql offset, toSql limit])
            where
                sql :: PrepQuery (Maybe PictDate, Maybe PictDate, Offset, Limit) [Pict]
                sql = fromString $ "SELECT " ++ basePictColumnList ++ " FROM bing_pict"
                    ++ "  LIMIT ?, ?"

        decode :: [[SqlValue]] -> [Pict]
        decode = decodeList decodePictFromSql

-- 查询一个月的所有记录
-- @param month 格式： 201709 :: Integer
selectPictListByMonth :: (IConnection conn) => conn -> Integer -> IO [Pict]
selectPictListByMonth = runSelect (compose sql encode) decode
    where
        sql :: PrepQuery Integer [Pict]
        sql = fromString $ "SELECT " ++ basePictColumnList ++ " FROM bing_pict"
            ++ " WHERE date > ? AND date < ?"

        encode :: Integer -> [SqlValue]
        encode date = [toSql $ date * 100, toSql $ date * 100 + 99]

        decode :: [[SqlValue]] -> [Pict]
        decode = decodeList decodePictFromSql

-- 查询最近一次记录的日期
selectLatestDate :: (IConnection conn) => conn -> () -> IO (Maybe PictDate)
selectLatestDate = runSelect (compose sql encode) decode
   where
       sql :: PrepQuery () (Maybe PictDate)
       sql = "SELECT MAX(date) as date FROM bing_pict"

       encode :: () -> [SqlValue]
       encode _ = []

       decode :: [[SqlValue]] -> Maybe PictDate
       decode = decodeOne fn
           where
               fn :: [SqlValue] -> Maybe PictDate
               fn (x:_) = Just (fromSql x)
               fn _ = Nothing
