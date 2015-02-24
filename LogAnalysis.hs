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
parse = (map parseMessage) . lines

-- Exercise 2.2

insert :: LogMessage -> MessageTree -> MessageTree
insert lm mt =
  case lm of
   Unknown _        -> mt
   LogMessage _ t _ -> mt
