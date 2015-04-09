{-# LANGUAGE TypeSynonymInstances #-}
module Calc where
import ExprT
import StackVM as S
import Parser

-- ex 1
eval :: ExprT -> Integer
eval (Lit n)     = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- ex 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

-- ex 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
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

instance Expr Mod7 where
  lit = Mod7 . flip mod 7
  add (Mod7 a) (Mod7 b) = lit (a + b)
  mul (Mod7 a) (Mod7 b) = lit (a * b)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

-- can also use GeneralizedNewtypeDeriving extension:
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- and add deriving (Num)
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
