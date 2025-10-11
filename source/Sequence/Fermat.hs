module Sequence.Fermat (fermatNumbers, fermatPrimes) where

import Prime (isPrime)

-- https://oeis.org/A000051
fermatNumbers :: (Integral a) => [a]
fermatNumbers = iterate (\x -> x * 2 - 1) 3

-- https://oeis.org/A019434
fermatPrimes :: (Integral a) => [a]
fermatPrimes = take 5 (filter isPrime fermatNumbers)

main :: IO ()
main = do
  print (take 15 fermatNumbers :: [Integer])
  print (fermatPrimes :: [Integer])
