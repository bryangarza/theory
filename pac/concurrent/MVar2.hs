module Main where

import Control.Concurrent

main = do
  m <- newEmptyMVar
  forkIO $ do
    putMVar m 'x'
    putMVar m 'y'
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r
