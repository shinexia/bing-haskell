{- |
   Copyright  : Copyright (C) 2017 Shine Xia (https://www.xgh.io)
   License    : Apache License 2.0
   Maintainer : Shine Xia <ishine.xia@gmail.com>
-}

module ConnPool
    ( newConnPool
    , withConn
    , delConnPool
    , trySql
    ) where


import Database.HDBC
import Database.HDBC.MySQL
import Data.Maybe (fromMaybe)
import Bing.Data.Pool

connConfig :: MySQLConnectInfo
connConfig = defaultMySQLConnectInfo {
            mysqlUser     = "root"
          , mysqlPassword = "1123586"
          , mysqlDatabase = "generaldb"
          , mysqlHost     = "127.0.0.1"
          }

main :: IO ()
main = do
    connPool <- newConnPool 0 50 (connectMySQL connConfig) disconnect

    withConn connPool $ \conn -> do
        r <- quickQuery' conn
             "SELECT id, 'desc' from test where id <= 1 or 1 = 1"
             []

        -- Convert each row into a String
        let stringRows = map convRow r

        -- Print the rows out
        mapM_ putStrLn stringRows

     where convRow :: [SqlValue] -> String
           convRow [sqlId, sqlDesc] =
               show intid ++ ": " ++ desc
               where intid = fromSql sqlId::Integer
                     desc = fromMaybe "NULL" (fromSql sqlDesc)
           convRow x = fail $ "Unexpected result: " ++ show x
