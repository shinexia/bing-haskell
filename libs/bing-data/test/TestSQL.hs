{- |
   Copyright  : Copyright (C) 2017 Shine Xia (https://www.xgh.io)
   License    : Apache License 2.0
   Maintainer : Shine Xia <ishine.xia@gmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.HDBC
import Database.HDBC.MySQL
import Bing.Data.SQL
import Bing.Data.Types
import Bing.Data.Util (withConnectionIO, setNamesUTF8)
import Data.Configurator (load, lookupDefault)
import Data.Configurator.Types (Config)
import qualified Data.Text.Lazy as TL

main :: IO ()
main = putStrLn "Hello World"

config :: IO Config
config = load ["test.conf"]

connConfig :: IO MySQLConnectInfo
connConfig = do
    conf <- config
    username <- lookupDefault "" conf "db_username"
    password <- lookupDefault "" conf "db_password"
    dbname   <- lookupDefault "" conf "db_name"
    dbhost   <- lookupDefault "" conf "db_host"
    -- putStrLn $ username ++ "," ++ password ++ "," ++ dbname ++ "," ++ dbhost
    return defaultMySQLConnectInfo {
              mysqlUser     = username
            , mysqlPassword = password
            , mysqlDatabase = dbname
            , mysqlHost     = dbhost
            }

connect :: IO Connection
connect = do
    conf <- connConfig
    conn <- connectMySQL conf
    _ <- setNamesUTF8 conn ()
    commit conn
    return conn

p :: Pict
p = Pict 20170913 "test url" "中文测试" "test copyright" "test copyrightlink"

testInsert :: Pict -> IO Integer
testInsert = withConnectionIO connect insertPict

testFind :: PictDate -> IO (Maybe Pict)
testFind = withConnectionIO connect selectOnePictByDate

testUpdate :: (PictDate, PictSavedPath) -> IO Integer
testUpdate = withConnectionIO connect updatePictSavedPath

testLatestDate :: IO (Maybe PictDate)
testLatestDate = withConnectionIO connect selectLatestDate ()

testMonth :: PictDate -> IO [Pict]
testMonth = withConnectionIO connect selectPictListByMonth
