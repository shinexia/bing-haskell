{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module Bing.Rest.Api where

import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Server (hoistServer)

import Bing.Data (PictDate, Offset, Limit)
import Bing.Rest.App (Env, getPort, AppIO, runAppIO, parseOpts, newEnv)
import qualified Bing.Rest.Image as Image
import Bing.Rest.Types

type Api_V1_Bing = "a" :> Get '[JSON] Int
      :<|> "b" :> Get '[JSON] String
      :<|> "images" :> QueryParam "date_from" PictDate
                          :> QueryParam "date_end" PictDate
                          :> QueryParam "idx" Offset
                          :> QueryParam "count" Limit
                          :> Get '[JSON] [PictDTO]

-- http://localhost:8081/api/v1/bing/images?count=4&idx=2&date_from=20171111
type Api = "api" :> "v1" :> "bing" :> Api_V1_Bing

serverT :: ServerT Api AppIO
serverT = a
     :<|> b
     :<|> Image.images
    where
        a :: AppIO Int
        a = return 1234

        b :: AppIO String
        b = return "hello, b"

api :: Proxy Api
api = Proxy

toHandler :: Env -> AppIO a -> Handler a
toHandler env r = liftIO $ runAppIO env r

server :: Env -> Server Api
server env = hoistServer api (toHandler env) serverT

app :: Env -> Application
app env = serve api (server env)

start :: IO ()
start = do
    opts <- parseOpts
    env <- newEnv opts
    let p = getPort env
    run p (app env)
