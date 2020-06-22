{- |
   Copyright  : Copyright (C) 2017 Shine Xia (https://www.xgh.io)
   License    : Apache License 2.0
   Maintainer : Shine Xia <ishine.xia@gmail.com>
-}

module Main where

import Data.Maybe (fromMaybe)
import Database.HDBC
import Database.HDBC.MySQL

main :: IO ()
main = putStrLn "Hello World"

connConfig :: MySQLConnectInfo
connConfig = defaultMySQLConnectInfo {
              mysqlUser     = "root"
            , mysqlPassword = "1123586"
            , mysqlDatabase = "generaldb"
            , mysqlHost     = "127.0.0.1"
            }

query :: Int -> IO ()
query maxId =
    do -- Connect to the database
       connect <- connectMySQL connConfig

       -- Run the query and store the results in r
       r <- quickQuery' connect
            "SELECT id, 'desc' from test where id <= 1 or 1 = 1"
            []

       -- Convert each row into a String
       let stringRows = map convRow r

       -- Print the rows out
       mapM_ putStrLn stringRows

       -- And disconnect from the database
       disconnect connect

    where convRow :: [SqlValue] -> String
          convRow [sqlId, sqlDesc] =
              show intid ++ ": " ++ desc
              where intid = fromSql sqlId::Integer
                    desc = fromMaybe "NULL" (fromSql sqlDesc)
          convRow x = fail $ "Unexpected result: " ++ show x
