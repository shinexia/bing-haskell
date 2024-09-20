{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Bing.Rest.App
    ( Opts
    , parseOpts
    , Env
    , getPort
    , newEnv
    , AppT
    , runAppT
    , AppIO
    , runAppIO
    , runApp
    , runSql
    ) where

import Control.Lens (makeLenses, view)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import Database.HDBC (commit)
import Database.HDBC.MySQL (Connection)

import Bing.Data.Connection (newConnectionFromEnv)

data Opts = Opts {
    optConfigPath :: String
  , optPort       :: Int
}

defaultOpts :: Opts
defaultOpts = Opts {
    optConfigPath = "test.conf"
  , optPort       = 8081
}

optsSpec :: [OptDescr (Opts -> Opts)]
optsSpec =
    [ Option ['c'] ["config"]
        (ReqArg (\f opts -> opts {optConfigPath = f}) "FILE")
        "config file path"
    , Option ['p'] ["port"]
        (ReqArg (\f opts -> opts {optPort = read f :: Int}) "PORT")
        "listen port"
    ]

parseOpts :: IO Opts
parseOpts = do
    argv <- getArgs
    progName <- getProgName
    let header = "Usage: " ++ progName ++ " OPTIONS"
    let helpMessage = usageInfo header optsSpec
    case getOpt Permute optsSpec argv of
        (o, [], []) -> return $ foldl (flip id) defaultOpts o
        (_, _, errs) -> ioError (userError $ concat errs ++ helpMessage)

data Env = Env {
    _port       :: Int
  , _connection :: Connection
}

makeLenses ''Env

getPort :: Env -> Int
getPort = view port

newEnv :: Opts -> IO Env
newEnv opts = do
    conn <- newConnectionFromEnv
    return $! Env {
        _connection = conn
      , _port       = optPort opts
    }

newtype AppT m a = AppT (ReaderT Env m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader Env
         )

runAppT :: Env -> AppT m a -> m a
runAppT env (AppT ma) = runReaderT ma env

type AppIO = AppT IO

runAppIO :: Env -> AppIO a -> IO a
runAppIO = runAppT

runApp :: AppIO a -> IO a
runApp fn = do
    opts <- parseOpts
    env <- newEnv opts
    runAppIO env fn

runSql :: (Connection -> a -> IO b) -> a -> AppIO b
runSql sql params = do
    conn <- view connection
    r <- liftIO $ sql conn params
    liftIO $ commit conn
    return r
