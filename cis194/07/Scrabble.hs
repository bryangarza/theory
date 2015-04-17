{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Scrabble where

import Data.Monoid

newtype Score = Score Int
              deriving(Eq, Ord, Num, Show)

getScore :: Score -> Int
getScore (Score i) = i

matchLetter :: Char -> Int
matchLetter c = case c of
  'a' -> 1
  'b' -> 3
  'c' -> 3
  'd' -> 2
  'e' -> 1
  'f' -> 4
  'g' -> 2
  'h' -> 4
  'i' -> 1
  'j' -> 8
  'k' -> 5
  'l' -> 1
  'm' -> 3
  'n' -> 1
  'o' -> 1
  'p' -> 3
  'q' -> 10
  'r' -> 1
  's' -> 1
  't' -> 1
  'u' -> 1
  'v' -> 4
  'w' -> 4
  'x' -> 8
  'y' -> 4
  'z' -> 10
  _   -> 0

class Scrabble a where
  score :: a -> Score
  scoreString :: a -> Score

instance Scrabble Char where
  score = Score . matchLetter

instance Scrabble String where
  scoreString = mconcat . map score

instance Monoid Score where
  mempty = Score 0
  mappend = (+)
