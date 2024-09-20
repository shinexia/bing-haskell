module Bing.Rest.Image
    ( images
    ) where

import Data.Maybe (fromMaybe)
import Bing.Data.SQL (selectPictListBetween)
import Bing.Data (PictDate, Offset, Limit)

import Bing.Rest.Types (makeDTO, PictDTO)
import Bing.Rest.App (runSql, AppIO)

images :: Maybe PictDate
        -> Maybe PictDate
        -> Maybe Offset
        -> Maybe Limit
        -> AppIO [PictDTO]
images date_from date_end index count = do
    let idx = fromMaybe 1 index
    let cnt = fromMaybe 20 count
    records <- runSql selectPictListBetween (date_from, date_end, idx, cnt)
    return $ map makeDTO records
