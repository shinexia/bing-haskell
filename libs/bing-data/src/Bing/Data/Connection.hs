{-# LANGUAGE OverloadedStrings #-}
module Bing.Data.Connection
    ( newConnection
    , newConnectionFromEnv
    , newDbConfigFromEnv
    ) where

import Data.Map (findWithDefault, fromList)
import Database.HDBC (commit)
import Database.HDBC.MySQL
import System.Environment (lookupEnv)

import Bing.Data.Types (Config)
import Bing.Data.Util (setNamesUTF8)

newConnection :: Config -> IO Connection
newConnection conf = do
    let dbhost   = findWithDefault "127.0.0.1" "db_mysql_host" conf
    let dbport   = read $ findWithDefault "3306" "db_mysql_port" conf :: Int
    let username = findWithDefault "root" "db_mysql_user" conf
    let password = findWithDefault "" "db_mysql_password" conf
    let dbname   = findWithDefault "demo" "db_mysql_database" conf
    -- putStrLn $ username ++ "," ++ password ++ "," ++ dbname ++ "," ++ dbhost
    let connInfo = defaultMySQLConnectInfo {
            mysqlHost       = dbhost
            , mysqlPort     = dbport
            , mysqlUser     = username
            , mysqlPassword = password
            , mysqlDatabase = dbname
            }
    conn <- connectMySQL connInfo
    _ <- setNamesUTF8 conn ()
    commit conn
    return conn

newConnectionFromEnv :: IO Connection
newConnectionFromEnv = newDbConfigFromEnv >>= newConnection

newDbConfigFromEnv :: IO Config
newDbConfigFromEnv = do
    keys <- setKeys [("DB_MYSQL_USER", "db_mysql_user", "root")
                   , ("DB_MYSQL_PASSWORD", "db_mysql_password", "")
                   , ("DB_MYSQL_HOST", "db_mysql_host", "127.0.0.1")
                   , ("DB_MYSQL_PORT", "db_mysql_port", "3306")
                   , ("DB_MYSQL_DATABASE", "db_mysql_database", "demo")]
    return $ fromList keys

setKeys :: [(String, String, String)] -> IO [(String, String)]
setKeys (x:xs) = do
    t <- setKey x
    tx <- setKeys xs
    return $ t ++ tx
setKeys _ = return []

setKey :: (String, String, String) -> IO [(String, String)]
setKey (a, b, c) = lookupEnv a >>= \m -> return $ fn m
    where
        fn :: Maybe String -> [(String, String)]
        fn m = case m of Just v -> [(b, v)]
                         _      -> [(b, c)]
