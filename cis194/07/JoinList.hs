{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Data.Monoid
import Buffer as B
import Sized as S
import Scrabble as Scr
import Editor as E

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty            = mempty
tag (Single ann _)   = ann
tag (Append ann _ _) = ann

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

(!!?) :: [a] -> Int -> Maybe a
[] !!? _        = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0    = Just x
(x:xs) !!? i    = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

branchSize :: (S.Sized b, Monoid b) => JoinList b a -> Int
branchSize = S.getSize . S.size . tag

indexJ :: (S.Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single m d)
  | i == 0    = Just d
  | otherwise = Nothing
indexJ i (Append m jl1 jl2)
  | i < 0                     = Nothing
  | i >= S.getSize (S.size m) = Nothing
  | i < s1                    = indexJ i jl1
  | otherwise                 = indexJ (i - s1) jl2
  where s1 = branchSize jl1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i jl@(Single _ _)
  | i <= 0    = jl
  | otherwise = Empty
dropJ i jl@(Append m jl1 jl2)
  | i <= 0    = jl
  | i < s1    = dropJ i jl1 +++ jl2
  | otherwise = dropJ (i - s1) jl2
  where s1 = branchSize jl1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i jl@(Single _ _)
  | i > 0     = jl
  | otherwise = Empty
takeJ i jl@(Append m jl1 jl2)
  | i <= 0    = Empty
  | i >= s1    = jl1 +++ takeJ (i - s1) jl2
  | otherwise = takeJ i jl1
  where s1 = branchSize jl1

scoreLine :: String -> JoinList Scr.Score String
scoreLine s = Single (Scr.scoreString s) s

instance B.Buffer (JoinList (Scr.Score, S.Size) String) where
  toString Empty          = ""
  toString (Single _ s)   = s
  toString (Append _ l r) = toString l ++ toString r

  fromString s = Single (Scr.scoreString s, S.Size 1) s

  line = indexJ

  replaceLine n s b = takeJ (n-1) b +++ fromString s +++ dropJ n b

  numLines Empty               = 0
  numLines (Single (_, n) _)   = getSize n
  numLines (Append (_, n) _ _) = getSize n

  value Empty                  = 0
  value (Single (n, _) _)      = Scr.getScore n
  value (Append (n, _) _ _)    = Scr.getScore n

main = E.runEditor E.editor $ Single (Score 1, Size 1) "f"
