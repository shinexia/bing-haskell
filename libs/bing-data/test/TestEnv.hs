{- |
   Copyright  : Copyright (C) 2017 Shine Xia (https://www.xgh.io)
   License    : Apache License 2.0
   Maintainer : Shine Xia <ishine.xia@gmail.com>
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

module TestEnv where


import Data.Text (Text)
import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader (ReaderT (..), runReaderT)


-- 测试 app monad 的使用方法

data Opts = Opts {
    _opt     :: Text
  , _other   :: Text
} deriving (Show)


data Env = Env {
    _name  :: Text
  , _username :: Text
  , _password :: Text
  , _number1  :: Integer
} deriving (Show)

makeLenses ''Env

readUsername :: IO Text
readUsername = return "shine"

readPassword :: IO Text
readPassword = return "password"

newEnv :: Opts -> IO Env
newEnv _ = do
    u <- readUsername
    p <- readPassword
    return $! Env
        { _name     = "" :: Text
        , _username = u
        , _password = p
        , _number1  = 10
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

checkNumber :: Integer -> AppIO Bool
checkNumber n = do
    base <- view number1
    if n < base then return True
        else return False

testOpts :: Opts
testOpts = Opts {_opt = "opt", _other = "other"}

testEnv :: Env
testEnv = Env
  { _name = "name"
  , _username = "shine"
  , _password = "password"
  , _number1 = 10
  }

result :: IO Bool
result = runAppT testEnv (checkNumber 1)
