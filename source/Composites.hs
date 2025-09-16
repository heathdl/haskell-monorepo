module Composites (compositesOfResidueClassModulo, composites, nonprimes, evenComposites, oddComposites) where

import Prime (primes)

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
