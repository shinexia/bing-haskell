{- |
   Copyright  : Copyright (C) 2017 Shine Xia (https://www.xgh.io)
   License    : Apache License 2.0
   Maintainer : Shine Xia <ishine.xia@gmail.com>
-}

{-# LANGUAGE OverloadedStrings #-}

module Bing.Bot.Api
    ( send
    ) where

import Network.HTTP.Simple
import Bing.Bot.Types (Result)

-- 域名
bingApiHostUrl :: String
bingApiHostUrl = "https://www.bing.com"

--
send :: Integer -> Integer -> IO (Int, Result)
send index count = do
    request <- parseRequest (bingApiHostUrl ++
        "/HPImageArchive.aspx?format=js&mkt=zh-CN&nc=1489309260264&pid=hp&video=1" ++
        "&idx=" ++ show index ++
        "&n=1" ++ show count)
    response <- httpJSON request
    let statusCode = getResponseStatusCode response
    let v = getResponseBody response :: Result
    return (statusCode, v)
