module Prime.Counting where

import Prime (primes)

primesLessThan :: (Integral a) => [a]
primesLessThan = 0 : go 1 primes
  where
    go count (p1 : ps@(p2 : _)) = replicate (p2 - p1) count ++ go (count + 1) ps

primesLessThanRoot :: (Integral a) => [a]
primesLessThanRoot = go primes 1 0
  where
    go ps@(p : ps') n count
      | p <= n = go ps' n (count + 1)
      | otherwise = replicate (2 * n + 1) count ++ go ps (n + 1) count