{- |
   Copyright  : Copyright (C) 2017 Shine Xia (https://www.xgh.io)
   License    : Apache License 2.0
   Maintainer : Shine Xia <ishine.xia@gmail.com>
-}

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
-- {-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
-- {-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StrictData                 #-}
-- {-# LANGUAGE TemplateHaskell            #-}

module TestEnv where


-- import Data.Text (Text)
-- import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Control.Monad.Reader
-- import Control.Monad.Reader.Class
-- import Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
-- import qualified Control.Monad.State.Strict as State


-- 测试泛型 app monad 的使用方法


-- in project A
-- class AEnv where
--     getNumber :: Integer

newtype LibEnv = LibEnv {getNumber :: Integer}

newtype LibT m a = LibT (ReaderT LibEnv m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader LibEnv
             )

runLibT :: LibEnv -> LibT m a -> m a
runLibT e (LibT ma) = runReaderT ma e

type LibIO = LibT IO

foo :: Integer -> LibIO Bool
foo n = do
    env <- ask
    let base = getNumber env
    return (n < base)

-- in project C
data AppEnv = AppEnv {
    _number :: Integer
  , _bool   :: Bool
}

newLibEnv :: AppEnv -> LibEnv
newLibEnv env = LibEnv {getNumber = _number env}

newtype AppT m a = AppT (ReaderT AppEnv m a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader AppEnv
             )

runAppT :: AppEnv -> AppT m a -> m a
runAppT e (AppT ma) = runReaderT ma e

type AppIO = AppT IO

withLibIO :: LibIO a -> AppIO a
withLibIO ma = do
    env <- ask
    let libEnv = newLibEnv env
    liftIO $ runLibT libEnv ma

bar :: Integer -> AppIO Integer
bar n = do
    ret <- withLibIO $ foo n
    return (if ret then 1 else 0)

testEnv :: AppEnv
testEnv = AppEnv {_number = 10, _bool = False}

resultC :: IO Integer
resultC = runAppT testEnv (bar 9)
