module Golf where
import Control.Lens
import Data.List (sort)
import Data.Bool (bool)

skips :: [a] -> [[a]]
skips [] = []
skips all@(_:xs) = all : (skips xs)

localMaxima :: [Integer] -> [Integer]
localMaxima = lm []

lm :: Ord a => [a] -> [a] -> [a]
lm maxs (a:b:c:xs)
  | b > a && b > c = lm (maxs++[b]) (b:c:xs)
  | otherwise = lm maxs (b:c:xs)
lm maxs _ = maxs

histogram :: [Integer] -> String
histogram xs = hist (sort xs) 0 0 0 ["          \n"]

hist :: [Integer] -> Int -> Int -> Int -> [String] -> String
hist [] _ _ _ out = concat $ reverse $ "==========\n0123456789\n" : out
hist (x:xs) h cur prev out =
  let xint = fromIntegral x
      hn   = bool (h + 1) 0 $ prev < xint
      outn = bool out (out ++ ["          \n"]) $ prev == xint
  in hist xs hn cur xint (outn & ix hn . ix xint .~ '*')
