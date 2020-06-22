{- |
   Copyright  : Copyright (C) 2017 Shine Xia (https://www.xgh.io)
   License    : Apache License 2.0
   Maintainer : Shine Xia <ishine.xia@gmail.com>
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Bing.Data.Util where

import Data.String
import Database.HDBC

--
newtype PrepQuery a b = PrepQuery
    { unQueryString :: String } deriving (Eq, Show)

--
instance IsString (PrepQuery a b) where
    fromString = PrepQuery

--
instance Monoid (PrepQuery a b) where
    mempty = fromString ""
    mappend x y = fromString $ unQueryString x ++ unQueryString y

--
type Offset = Integer
--
type Limit  = Integer

--
runSelect :: IConnection c
       => (a -> (PrepQuery a b, [SqlValue]))
       -> ([[SqlValue]] -> b)
       -> c
       -> a
       -> IO b
runSelect encode decode conn param = do
    let (sql, bindParams) = encode param
    r <- quickQuery' conn (unQueryString sql) bindParams
    return $ decode r

--
runExecute :: IConnection c
       => (a -> (PrepQuery a Integer, [SqlValue]))
       -> c
       -> a
       -> IO Integer
runExecute encode conn params = do
    let (sql, bingParams) = encode params
    stmt <- prepare conn (unQueryString sql)
    execute stmt bingParams

--
setNamesUTF8 :: IConnection conn => conn -> () -> IO Integer
setNamesUTF8 = runExecute (const (sql, []))
   where
       sql :: PrepQuery () Integer
       sql = "SET NAMES UTF8"

--
withConnectionIO :: IConnection c => IO c -> (c -> a -> IO b) -> a -> IO b
withConnectionIO ioc f a = do
    conn <- ioc
    withConnection conn f a

--
withConnection :: IConnection c => c -> (c -> a -> IO b) -> a -> IO b
withConnection conn f a = do
    r <- f conn a
    commit conn
    return r

--
encodeEmpty :: a -> [SqlValue]
encodeEmpty _ = []

--
compose :: PrepQuery a b -> (a -> [SqlValue]) -> (a -> (PrepQuery a b, [SqlValue]))
compose sql encode t = (sql, encode t)

--
decodeList :: ([SqlValue] -> Maybe b) -> [[SqlValue]] -> [b]
decodeList = fn []
    where
        fn :: [b] -> ([SqlValue] -> Maybe b) -> [[SqlValue]] -> [b]
        fn acc f (x:rest) = do
            let r = f x
            case r of
                Just y -> fn (acc ++ [y]) f rest
                Nothing -> fn acc f rest
        fn acc _ _ = acc

--
decodeOne :: ([SqlValue] -> Maybe b) -> [[SqlValue]] -> Maybe b
decodeOne df (x:_) = df x
decodeOne _ _ = Nothing
