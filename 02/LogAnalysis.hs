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
insert (Unknown _) mt = mt
insert lm Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ t _) (Node left msg@(LogMessage _ tsmsg _) right)
  | t < tsmsg = Node (insert lm left) msg right
  | otherwise = Node left msg (insert lm right)

-- Exercise 2.3

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

-- Exercise 2.4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

-- Exercise 2.5

getErrorSeverity :: LogMessage -> Int
getErrorSeverity (LogMessage (Error n) _ _) = n
getErrorSeverity _ = 0

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ msg) = msg

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = map (getMessage) $
                   filter (\x -> getErrorSeverity x > 50) $
                   inOrder (build xs)
