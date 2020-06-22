module TestRegx where

import Bing.Task.Util

path :: String
path = "https://www.bing.com/az/hprichbg/rb/LanseMeadows_ZH-CN10703907742_1920x1080.jpg"

results :: (String, String)
results = getOriginUrlAndSavedPath path 20170918
