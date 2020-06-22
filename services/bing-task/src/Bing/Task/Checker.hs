{- |
   Copyright  : Copyright (C) 2017 Shine Xia (https://xgh.io)
   License    : Apache License 2.0
   Maintainer : Shine Xia <shine.xgh@gmail.com>
-}

module Bing.Task.Checker
    ( runChecker
    )where


import Control.Monad.IO.Class (liftIO)
import Control.Lens
import Database.HDBC
import Database.HDBC.MySQL
import qualified Data.Text.Lazy as L
import Bing.Data
import Bing.Task.Util
import Bing.Task.App


saveImage :: Connection -> Pict -> AppIO ()
saveImage conn pict = do
    let url = pictOriginUrl pict
    let filename = getFileName $ L.unpack url
    let date = pictDate pict :: Integer
    let subdir = generateSavePath date
    fullurl  <- fmap  (++ L.unpack url) (view bingBaseUrl)
    savedir <- fmap (++ subdir) (view saveBaseDir)
    -- liftIO $ putStrLn $ fullurl ++ "\n" ++ savepath ++ "\n" ++ url
    liftIO $ download fullurl savedir filename
    _ <- liftIO $ updatePictSavedPath conn (date, L.pack $ subdir ++ "/" ++ filename)
    _ <- liftIO $ commit conn
    return ()


runChecker :: AppIO ()
runChecker = do
    conn <- view connection
    picts <- liftIO $ withConnection conn selectPictListOfEmptySavedPath ()
    mapM_  (saveImage conn) picts
    return ()
