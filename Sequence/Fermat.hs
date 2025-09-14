module Sequence.Fermat (fermatNumbers, fermatPrimes) where

import Prime (isPrime)

fermatNumbers :: (Integral a) => [a]
fermatNumbers = iterate (\x -> x * 2 - 1) 3

fermatPrimes :: (Integral a) => [a]
fermatPrimes = take 5 (filter isPrime fermatNumbers)

main :: IO ()
main = do
  print (take 15 fermatNumbers :: [Integer])
  print (fermatPrimes :: [Integer])