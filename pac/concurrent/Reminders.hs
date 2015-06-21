module Main where

import Control.Concurrent
import Text.Printf
import Control.Monad

main =
  forever $ do
    s <- getLine
    forkIO $ setReminder s

setReminder :: String -> IO ()
setReminder s = do
  let t = read s :: Int
  printf "Ok, I'll remind you in %d seconds\n" t
  threadDelay (10^6 * t)
  printf "%d seconds is up!\n" t
