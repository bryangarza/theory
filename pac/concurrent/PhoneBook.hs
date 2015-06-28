module Main where

import Control.Concurrent
import qualified Data.Map as Map
import Prelude hiding (lookup)

type Name        = String
type PhoneNumber = String
type PhoneBook   = Map.Map Name PhoneNumber

newtype PhoneBookState = PhoneBookState (MVar PhoneBook)

new :: IO PhoneBookState
new = do
  m <- newMVar Map.empty
  return (PhoneBookState m)

insert :: PhoneBookState -> Name -> PhoneNumber -> IO ()
insert (PhoneBookState m) name number = do
  book <- takeMVar m
  -- putMVar m (Map.insert name number book)
  -- could also do:
  -- `$!` evals the arg strictly before applying the function
  -- putMVar m $! Map.insert name number book
  -- To get brief locking and no space leaks, we need to use a trick:
  let book' = Map.insert name number book
  putMVar m book'
  seq book' (return ())

lookup :: PhoneBookState -> Name -> IO (Maybe PhoneNumber)
lookup (PhoneBookState m) name = do
  book <- takeMVar m
  putMVar m book
  return (Map.lookup name book)

main = do
  s <- new
  sequence_ [ insert s ("name" ++ show n) (show n) | n <- [1..10000] ]
  lookup s "name999" >>= print
  lookup s "unknown" >>= print
