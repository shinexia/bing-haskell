module TestCron where

import Control.Concurrent
import Control.Monad
import Data.Attoparsec.Text
import Data.Monoid (mconcat)
import Data.Text (Text, pack, unpack, splitOn)
import Data.Time.Clock
import qualified Data.Text.IO as TIO
import System.Cron
import System.Cron.Parser
import System.Environment
import System.Posix.Process
import System.Posix.Types (ProcessID)

main :: IO ()
main = forever $ run >> sleep
    where
        sleep      = putStrLn "Sleeping" >> threadDelay delay
        delay      = 60 * 100000


run :: IO ()
run = do
    tids <- execSchedule $
        addJob job1 "* * * * *"
    print tids


job1 :: IO ()
job1 = putStrLn "job 1"
-- 
-- runCommand :: String -> String -> IO ProcessID
-- runCommand env cmd = fork execute
--   where fork io = log >> forkProcess io
--         execute = executeFile cmdPath True args (Just env)
--         (cmdPath:args) = map unpack $ splitOn " " cmd
--         log = TIO.putStrLn . mconcat $ ["Running ", cmd, " with ", (pack . show) env]
