{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}
module Notes where

data Foo = F Int | G Char

instance Eq Foo where
  (F i1) == (F i2) = i1 == i2
  (G c1) == (G c2) = c1 == c2
  _ == _ = False

  foo1 /= foo2 = not (foo1 == foo2)


-- or, just derive:
data Foo' = F' Int | G' Char
          deriving (Eq, Ord, Show)


-- a type class example:
class Listable a where
  toList :: a -> [Int]

instance Listable Int where
  -- toList :: Int -> [Int]
  toList x = [x]

instance Listable Bool where
  toList True  = [1]
  toList False = [0]

instance Listable [Int] where
  toList = id

data Tree a = Empty | Node a (Tree a) (Tree a)

instance Listable (Tree Int) where
  toList Empty = []
  toList (Node x l r) = toList l ++ [x] ++ toList r

-- to compute sumL, first convert to a list of Ints, then sum
sumL :: Listable a => a -> Int
sumL x = sum (toList x)

foo :: (Ord a, Listable a) => a -> a -> Bool
foo x y = sum (toList x) == sum (toList y) || x < y

instance (Listable a, Listable b) => Listable (a,b) where
  toList (x,y) = toList x ++ toList y

-- Notice how we can put type class constraints on an instance as well as
-- on a function type. This says that a pair type `(a,b)` is an instance
-- of `Listable` as long as a and b both are. Then we get to use `toList`
-- on values of types `a` and `b` in our definition of `toList` for a
-- pair. Note that this definition is *not* recursive! The version of
-- `toList` that we are defining is calling other versions of `toList`,
-- not itself.
