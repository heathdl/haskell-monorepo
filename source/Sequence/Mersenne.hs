module Sequence.Mersenne (mersenneNumbers, mersennePrimes) where

import Prime (isPrime)

mersenneNumbers :: (Integral a) => [a]
mersenneNumbers = iterate (\x -> x * 2 + 1) 3

mersennePrimes :: (Integral a) => [a]
mersennePrimes = filter isPrime mersenneNumbers

main :: IO ()
main = do
  print (take 15 mersenneNumbers :: [Integer])
  print (mersennePrimes :: [Integer])