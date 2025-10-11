module Sequence.FreestylePerfect where

import Factors (nonPrimeFactorisations, nonPrimeFactorisationsFromFactorisation)
import Multiset (Multiset)
import Prime.Factorisation (primeFactorisation)
import Prime.Factorisation.Pairwise (primeFactorPairs)
import Sequence.Perfect (isPerfectNumberFromFactorisation)

isFreestylePerfectNumberFromFactorisation :: (Integral a) => a -> Multiset a -> Bool
isFreestylePerfectNumberFromFactorisation x y = any (isPerfectNumberFromFactorisation x) (nonPrimeFactorisationsFromFactorisation y)

isFreestylePerfectNumber :: (Integral a) => a -> Bool
isFreestylePerfectNumber x = isFreestylePerfectNumberFromFactorisation x (primeFactorisation x)

-- https://oeis.org/A058007
freestylePerfectNumbers :: (Integral a) => [a]
freestylePerfectNumbers = map fst (filter (uncurry isFreestylePerfectNumberFromFactorisation) primeFactorPairs)

main :: IO ()
main = do
  print freestylePerfectNumbers
