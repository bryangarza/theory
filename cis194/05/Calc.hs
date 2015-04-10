{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Calc where
import ExprT as E
import StackVM as S
import Parser

-- ex 1
eval :: ExprT -> Integer
eval (E.Lit n)     = n
eval (E.Add e1 e2) = eval e1 + eval e2
eval (E.Mul e1 e2) = eval e1 * eval e2

-- ex 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp E.Lit E.Add E.Mul

-- ex 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr E.ExprT where
  lit = E.Lit
  add = E.Add
  mul = E.Mul

reify :: E.ExprT -> E.ExprT
reify = id

--ex 4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
instance Expr MinMax where
  lit = MinMax
  add = min
  mul = max

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit = Mod7 . flip mod 7
  add (Mod7 a) (Mod7 b) = lit (a + b)
  mul (Mod7 a) (Mod7 b) = lit (a * b)

-- can also use GeneralizedNewtypeDeriving extension:
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- and add deriving (Num):
--
-- newtype Mod7 = Mod7 Integer deriving (Eq, Show, Num)
--
-- instance Expr Mod7 where
--   lit = Mod7 . flip mod 7
--   add = (+)
--   mul = (*)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- ex 5
-- For any arithmetic expression exp :: Expr a => a it should be the case that:
--     stackVM exp == Right [IVal exp]
instance Expr S.Program where
  lit = (:[]) . S.PushI
  add x y = x ++ y ++ [S.Add]
  mul x y = x ++ y ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul
