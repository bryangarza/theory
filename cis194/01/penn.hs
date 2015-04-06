module Example where
import Text.Read (read)
import GHC.Show (show)
import GHC.List (reverse, zipWith)
import Data.Char (digitToInt)
import GHC.Real (fromIntegral)

-- Note: Examples from LYAH and RWH are included in this source file as well.

-- Week 1

square :: Integral a => a -> a
square x = x * x

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise = 3*n + 1

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

lineCount :: String -> Int
lineCount = length . lines

wordCount :: String -> Int
wordCount = length . words

myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop n l@(_:xs)
  | n < 1     = l
  | otherwise = myDrop (n - 1) xs

-- Homework 1

-- Exercise 1.1

toDigits :: Integer -> [Integer]
toDigits = map (read . (:[])) . show

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- Exercise 1.2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) (cycle [1,2]) . reverse

-- Exercise 1.3

splitDigits :: [Char] -> [Integer]
splitDigits = map (fromIntegral .  digitToInt)

joinIntegers :: [Integer] -> [Char]
joinIntegers xs = filter (/= ',') $ init $ tail $ show xs

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ splitDigits (joinIntegers xs)

-- Exercise 1.4

validate :: Integer -> Bool
validate n = mod (sumDigits $ doubleEveryOther $ toDigits n) 10 == 0

-- Exercise 1.5

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = hanoi (pred n) a c b ++ [(a, b)] ++ hanoi (pred n) c b a


-- Week 2

data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

-- better:

-- type synonyms
type CustomerID = Int
type ReviewBody = String

-- Value constructor has the same name as its type constructor.
data BookReview = BookReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)


-- Algebraic data types

type CardHolder = String
type CardNumber = String
type Address = [String]

-- 3 ways to bill customers
data BillingInfo = CreditCard CardNumber CardHolder Address
                   | CashOnDelivery
                   | Invoice CustomerID
                     deriving (Show)

-- Differentiating between identical info used for different things:

-- x and y coordinates or lengths
data Cartesian2D = Cartesian2D Double Double
                   deriving (Eq, Show)

-- Angle and distance (magnitude)
data Polar2D = Polar2D Double Double
               deriving (Eq, Show)

-- Record syntax
data Customer = Customer {
  customerID      :: Int,
  customerName    :: String,
  customerAddress :: Address
  } deriving (Show)

customer1 = Customer 271828 "J.R. Hacker"
            ["255 Syntax Ct",
             "Milpitas, CA 95314",
             "USA"]

customer2 = Customer {
  customerID      = 271828,
  customerAddress = ["1048576 Disk Drive",
                     "Milpitas, CA 95314",
                     "USA"],
  customerName    = "Jane Q. Citizen"
  }


-- Parametrized types

-- for example (defined in Prelude, Data.Maybe):
-- data Maybe a = Nothing | Just a

-- "We usually use type parameters when the type that's contained inside
-- the data type's various value constructors isn't really that important
-- for the type to work. A list of stuff is a list of stuff and it
-- doesn't matter what the type of that stuff is, it can still work. If
-- we want to sum a list of numbers, we can specify later in the summing
-- function that we specifically want a list of numbers. Same goes for
-- Maybe. Maybe represents an option of either having nothing or having
-- one of something. It doesn't matter what the type of that something
-- is." - http://learnyouahaskell.com/making-our-own-types-and-typeclasses

-- Recursive types

data List a = Cons a (List a)
            | Nil
              deriving (Show)

-- Our List type and the built-in [a] are isomorphic.

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

toList (Cons x xs) = x : toList xs
toList Nil = []

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)
firstTree = Node "parent" Empty (Node "right child" Empty Empty)


data NewTree a = NewNode a (Maybe (NewTree a)) (Maybe (NewTree a))
            deriving (Show)
simpleTree = NewNode "parent" Nothing (Just (NewNode "right" Nothing Nothing))

-- Reporting errors

mySecond :: [a] -> a
mySecond xs = if null (tail xs)
              then error "list too short"
              else head (tail xs)

safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
                then Nothing
                else Just (head (tail xs))

tidySecond :: [a] -> Maybe a
tidySecond (_:x:_) = Just x
tidySecond _       = Nothing


lend3 amount balance
  | amount <= 0             = Nothing
  | amount > reserve  * 0.5 = Nothing
  | otherwise               = Just newBalance
  where reserve    = 100
        newBalance = balance - amount

-- Note: Homework 2 is in LogAnalysis.hs
