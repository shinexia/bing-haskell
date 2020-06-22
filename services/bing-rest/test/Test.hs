{-# LANGUAGE OverloadedStrings #-}
module Test
    (
    ) where


import Bing.Data.Types
import Bing.Rest.Types

p = Pict "20170905" "/url_test" "/path_test" "copyright_test" "/copyrightlink_test":: Pict

dto = makeDTO p :: PictDTO
