module Main where

import Control.Concurrent

-- BlockedIndefinitelyOnMVar
-- Deadlock, runtime throws exception
main = do
  m <- newEmptyMVar
  takeMVar m
