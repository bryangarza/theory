module Party where

import Data.List (intercalate)
import Data.Tree
import Employee


glCons :: Employee -> GuestList -> GuestList
glCons emp (GL es fun) = GL (emp : es) $ fun + empFun emp

instance Monoid GuestList where
  mempty = GL [] 0
  (GL es1 fun1) `mappend` (GL es2 fun2) = GL (es1 ++ es2) $ fun1 + fun2

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a ts) = f a $ map (treeFold f) ts

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gs = (w, wo) where
  w  = glCons e (mconcat $ map snd gs)
  wo = mconcat $ map (uncurry moreFun) gs

maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . treeFold nextLevel

funListFormat :: GuestList -> String
funListFormat (GL es fun) = unlines $ ("Fun level: " ++ show fun) : l
  where l = map empName es

main :: IO ()
main = do
  t <- readFile "company.txt"
  putStrLn $ funListFormat $ maxFun $ read t
