module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream e rest) = e : streamToList rest

instance Show a => Show (Stream a) where
  show = concatMap ((++",") . show) . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Stream x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream e rest) = Stream (f e) $ streamMap f rest

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x $ streamFromSeed f $ f x

nats :: Stream Integer
nats = streamFromSeed succ 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream a as) (Stream b bs) =
  Stream a $ Stream b $ interleaveStreams as bs

interleaveStreams4 :: Stream a -> Stream a -> Stream a -> Stream a -> Stream a
interleaveStreams4 (Stream a as) (Stream b bs) (Stream c cs) (Stream d ds) =
  Stream a $ Stream b $ Stream c $ Stream d $ interleaveStreams4 as bs cs ds

ruler :: Stream Integer
ruler =
  let zeros  = streamRepeat 0
      ones   = streamRepeat 1
      twos   = streamRepeat 2
      threes = streamRepeat 3
      fours  = streamRepeat 4
      pows   = interleaveStreams4 twos threes twos fours
  in interleaveStreams4 zeros ones zeros pows
