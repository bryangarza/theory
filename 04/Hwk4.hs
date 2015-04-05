module Hwk4 where

-- Rewrite using *wholemeal programming* practices

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr ((*) . (subtract 2)) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate iter
  where iter n
          | even n    = n `div` 2
          | otherwise = 3 * n + 1


data Tree a = Leaf
              | Node Integer (Tree a) a (Tree a)
              deriving (Show, Eq)

depth :: Tree a -> Integer
depth Leaf = 0
depth (Node h _ _ _) = h

insert :: a -> Tree a -> Tree a
insert e Leaf = Node 0 Leaf e Leaf
insert e (Node _ Leaf e' r) = Node (depth r + 1) (insert e Leaf) e' r
insert e (Node _ l e' Leaf) = Node (depth l + 1) l e' (insert e Leaf)
insert e (Node _ l e' r) =
  let lins = insert e l
      rins = insert e r
  in
   if depth l < depth r
   then Node (depth lins + 1) lins e' r
   else Node (depth rins + 1) l e' rins

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
