{- |
   Copyright  : Copyright (C) 2017 Shine Xia (https://xgh.io)
   License    : Apache License 2.0
   Maintainer : Shine Xia <shine.xgh@gmail.com>
-}

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TemplateHaskell            #-}


module Bing.Task.App where

import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader (ReaderT (..), runReaderT)

import Database.HDBC
import Database.HDBC.MySQL
import Bing.Data (newConnectionFromEnv)
import System.Console.GetOpt
import System.Environment(getArgs, getProgName)


data Opts = Opts {
    optConfigPath   :: String
  , optOutputDir    :: String
  , optCheckUnsaved :: Bool
  } deriving Show

defaultOpts :: Opts
defaultOpts = Opts {
    optConfigPath   = "bing-task.conf"
  , optOutputDir    = "/data/www/bing"
  , optCheckUnsaved = False
  }

options :: [OptDescr (Opts -> Opts)]
options =
    [ Option ['c'] ["conf"]
        (ReqArg (\ f opts -> opts {optConfigPath = f}) "FILE")
        "config file path"
    , Option ['o'] ["output"]
        (ReqArg (\ d opts -> opts {optOutputDir  = d}) "DIR")
        "output dir"
    , Option [] ["check"]
        (NoArg (\ opts -> opts {optCheckUnsaved  = True}))
        "check unsaved"
    ]

parseOpts :: IO Opts
parseOpts = do
  argv <- getArgs
  progName <- getProgName
  let header = "Usage: " ++ progName ++ " [OPTION...]"
  let helpMessage = usageInfo header options
  case getOpt Permute options argv of
    (o, [], []) -> return $ foldl (flip id) defaultOpts o
    (_, _, errs)   -> ioError (userError (concat errs ++ helpMessage))

data Env = Env {
    _connection    :: Connection
  , _bingBaseUrl   :: String
  , _saveBaseDir   :: String
}

makeLenses ''Env

newEnv :: Opts -> IO Env
newEnv opts = do
    conn <- newConnectionFromEnv
    return $! Env
        { _connection    = conn
        , _bingBaseUrl   = "https://www.bing.com"
        , _saveBaseDir   = optOutputDir opts
        }

newtype AppT m a = AppT (ReaderT Env m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader Env
             )

type AppIO = AppT IO

runAppT :: Env -> AppT m a -> m a
runAppT e (AppT ma) = runReaderT ma e

runOpts :: Opts -> AppIO () -> IO ()
runOpts opts fn = do
    env <- newEnv opts
    runAppT env fn

runDb :: (Connection -> a -> IO b) -> a -> AppIO b
runDb f  a = do
    conn <- view connection
    b <- liftIO $ f conn a
    liftIO $ commit conn
    return b
