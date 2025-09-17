module Composites (compositesOfResidueClassModulo, composites, nonprimes, evenComposites, oddComposites, highlyCompositeNumbers, largelyCompositeNumbers) where

import Factors (countFactors)
import Prime (primes)
import Prime.Factorisation.Pairwise (primeFactorPairs)

compositesOfResidueClassModulo :: (Integral a) => a -> a -> [a]
compositesOfResidueClassModulo n m
  | n < 2 = go (n + m) primes
  | otherwise = go n primes
  where
    go x p@(ph : ps)
      | ph < x = go x ps
      | ph == x = go x' ps
      | otherwise = x : go x' p
      where
        x' = x + m

composites :: (Integral a) => [a]
composites = compositesOfResidueClassModulo 0 1

nonprimes :: (Integral a) => [a]
nonprimes = 1 : composites

evenComposites :: (Integral a) => [a]
evenComposites = compositesOfResidueClassModulo 0 2

oddComposites :: (Integral a) => [a]
oddComposites = compositesOfResidueClassModulo 1 2

highlyCompositeNumbers :: (Integral a) => [a]
highlyCompositeNumbers = nextHighlyComposite primeFactorPairs 0
  where
    nextHighlyComposite ((x, factors) : xs) currentHighest
      | factorCount > currentHighest = x : nextHighlyComposite xs factorCount
      | otherwise = nextHighlyComposite xs currentHighest
      where
        factorCount = countFactors factors

largelyCompositeNumbers :: (Integral a) => [a]
largelyCompositeNumbers = nextLargelyComposite primeFactorPairs 0
  where
    nextLargelyComposite ((x, factors) : xs) currentHighest
      | factorCount >= currentHighest = x : nextLargelyComposite xs factorCount
      | otherwise = nextLargelyComposite xs currentHighest
      where
        factorCount = countFactors factors
