{- |
   Copyright  : Copyright (C) 2017 Shine Xia (https://xgh.io)
   License    : Apache License 2.0
   Maintainer : Shine Xia <shine.xgh@gmail.com>
-}

module Bing.Task.Worker where

import Control.Monad (when, fmap)
import Control.Monad.IO.Class (liftIO)
import Control.Lens
import Database.HDBC
import Database.HDBC.MySQL
import qualified Data.Text.Lazy as L
import Bing.Bot
import Bing.Data
import Bing.Task.Util
import Bing.Task.App


saveImage :: Connection -> Image -> AppIO ()
saveImage conn image = do
    let url = _url image
    let filename = getFileName $ L.unpack url
    let date = readDate $ _enddate image
    let subdir = generateSavePath date
    fullurl  <- fmap  (++ L.unpack url) (view bingBaseUrl)
    savedir <- fmap (++ subdir) (view saveBaseDir)
    -- liftIO $ putStrLn $ fullurl ++ "\n" ++ savepath ++ "\n" ++ url
    liftIO $ download fullurl savedir filename
    let pict = imageToPict (L.pack $ subdir ++ "/" ++ filename) image
    -- liftIO $ print (encode image)
    -- liftIO $ print (encode pict)
    _ <- liftIO $ insertPict conn pict
    _ <- liftIO $ commit conn
    return ()


runWorker :: AppIO ()
runWorker = do
    conn <- view connection
    (_, result) <- liftIO $ send 0 10
    let images  = _images result
    maxdate <- liftIO $ selectLatestDate conn ()
    let md = getmd maxdate
    let fn image = when (readDate (_enddate image) > md) (saveImage conn image)
    mapM_ fn images
    return ()
        where
            getmd date = case date of
                Just x -> x
                _      -> 0
