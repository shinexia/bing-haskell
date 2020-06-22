{- |
   Copyright  : Copyright (C) 2017 Shine Xia (https://www.xgh.io)
   License    : Apache License 2.0
   Maintainer : Shine Xia <ishine.xia@gmail.com>
-}

module Bing.Data.Pool
    ( Pool
    , newConnPool
    , withConn
    , delConnPool
    , trySql
    , trySqlWithConn
    ) where

import Control.Concurrent
import Control.Exception
import Control.Monad (replicateM)
import Database.HDBC


data PoolInfo a =
    PoolInfo { poolMin :: Int, poolMax :: Int, poolUsed :: Int, poolFree :: [a] }


type Pool a = (MVar (PoolInfo a), IO a, a -> IO ())


newConnPool :: Int -> Int -> IO a -> (a -> IO ()) -> IO (Pool a)
newConnPool low high newConn delConn = do
--    cs <- handleSqlError . sequence . replicate low newConn
    cs <- replicateM low newConn
    mPool <- newMVar $ PoolInfo low high 0 cs
    return (mPool, newConn, delConn)


delConnPool :: Pool a -> IO ()
delConnPool (mPool, _, delConn) = do
    pool <- takeMVar mPool
    if length (poolFree pool) /= poolUsed pool
      then putMVar mPool pool >> fail "pool in use"
      else mapM_ delConn $ poolFree pool


takeConn :: Pool a -> IO a
takeConn (mPool, newConn, _) = modifyMVar mPool $ \pool ->
    case poolFree pool of
        conn:cs ->
            return (pool { poolUsed = poolUsed pool + 1, poolFree = cs }, conn)
        _ | poolUsed pool < poolMax pool -> do
            conn <- handleSqlError newConn
            return (pool { poolUsed = poolUsed pool + 1 }, conn)
        _ -> fail "pool is exhausted"


putConn :: Pool a -> a -> IO ()
putConn (mPool, _, delConn) conn = modifyMVar_ mPool $ \pool ->
    let used = poolUsed pool in
    if used > poolMin pool
    then handleSqlError (delConn conn) >> return (pool { poolUsed = used - 1 })
    else return $ pool { poolUsed = used - 1, poolFree = conn : poolFree pool }


withConn :: Pool a -> (a -> IO c) -> IO c
withConn connPool = bracket (takeConn connPool) (putConn connPool)


trySql :: IConnection a => a -> (a -> IO c) -> IO c
trySql conn f = handleSql catcher $ do
    r <- f conn
    commit conn
    return r
  where catcher e = rollback conn >> throw e


trySqlWithConn :: IConnection a => Pool a -> (a -> IO c) -> IO c
trySqlWithConn connPool f = bracket (takeConn connPool) (putConn connPool) $ \conn ->
    trySql conn f
