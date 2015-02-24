{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 2.1

parseMessage :: String -> LogMessage
parseMessage s =
  case xs of
   ("I":a:bs)   -> LogMessage Info (read a) (unwords bs)
   ("W":a:bs)   -> LogMessage Warning (read a) (unwords bs)
   ("E":a:b:cs) -> LogMessage (Error $ read a) (read b) (unwords cs)
   _            -> Unknown s
  where xs = words s

parse :: String -> [LogMessage]
parse xs = map (parseMessage) (lines xs)
