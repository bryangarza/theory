module Golf where

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
