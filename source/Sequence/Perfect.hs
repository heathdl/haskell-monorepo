module Sequence.Perfect where

import Multiset (Multiset, multiSetDoubleMap)
import Prime.Factorisation (primeFactorisation)
import Sequence.Mersenne

isPerfectNumberFromFactorisation :: (Integral a) => a -> Multiset a -> Bool
isPerfectNumberFromFactorisation x f = sumOfDivisors f == 2 * x
  where
    sumOfDivisors = multiSetDoubleMap mapFactor product
    mapFactor f e = (f ^ (e + 1) - 1) `div` (f - 1)

isPerfectNumber :: (Integral a) => a -> Bool
isPerfectNumber x = isPerfectNumberFromFactorisation x (primeFactorisation x)

perfectNumbers :: (Integral a) => [a]
perfectNumbers = map (\n -> (n * (n + 1)) `div` 2) mersennePrimes

main :: IO ()
main = do
  print perfectNumbers
