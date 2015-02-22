module Example where
import Text.Read (read)
import GHC.Show (show)
import GHC.List (reverse, zipWith)
import Data.Char (digitToInt)
import GHC.Real (fromIntegral)

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

-- Exercise 1

toDigits :: Integer -> [Integer]
toDigits = map (read . (:[])) . show

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- Exercise 2

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) (cycle [1,2]) . reverse

-- Exercise 3

splitDigits :: [Char] -> [Integer]
splitDigits = map (fromIntegral .  digitToInt)

joinIntegers :: [Integer] -> [Char]
joinIntegers xs = filter (/= ',') $ init $ tail $ show xs

sumDigits :: [Integer] -> Integer
sumDigits xs = foldl (+) 0 $ splitDigits (joinIntegers xs)

-- Exercise 4

validate :: Integer -> Bool
validate n = mod (sumDigits $ doubleEveryOther $ toDigits n) 10 == 0

-- Exercise 5

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
