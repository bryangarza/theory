module Party where

import Data.Tree
import Employee


glCons :: Employee -> GuestList -> GuestList
glCons emp (GL es fun) = GL (emp : es) $ fun + empFun emp

instance Monoid GuestList where
  mempty = GL [] 0
  (GL es1 fun1) `mappend` (GL es2 fun2) = GL (es1 ++ es2) $ fun1 + fun2

moreFun :: GuestList -> GuestList -> GuestList
moreFun x@(GL _ funx) y@(GL _ funy)
  | funx > funy = x
  | otherwise   = y

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold f acc t = foldr f acc (flatten t)
