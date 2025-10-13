module Prime.Totients (eulersTotient, eulersTotientGivenFactorisation, carmichaelsTotient, carmichaelsTotientGivenFactorisation, carmichaelsTotientOfSemiprimeGivenFactors) where

import Multiset (Multiset)
import Prime.Factorisation (primeFactorisation)

eulersTotientGivenFactorisation :: (Integral a) => Multiset a -> a
eulersTotientGivenFactorisation factorisation = product (map (\(factor, count) -> (factor - 1) * factor ^ (count - 1)) factorisation)

eulersTotient :: (Integral a) => a -> a
eulersTotient = eulersTotientGivenFactorisation . primeFactorisation

carmichaelsTotientGivenFactorisation :: (Integral a) => Multiset a -> a
carmichaelsTotientGivenFactorisation factorisation
  | null factorisation || factorisation == [(2, 1)] || factorisation == [(2, 2)] = totient
  | isPowerOfTwo = totient `div` 2
  | isPowerOfPrime = totient
  | otherwise = leastCommonMultiple (map (\x -> carmichaelsTotientGivenFactorisation [x]) factorisation)
  where
    isPowerOfPrime = null (tail factorisation)
    isPowerOfTwo = case factorisation of
      [(2, c)] -> c >= 3
      _ -> False
    totient = eulersTotientGivenFactorisation factorisation

    leastCommonMultiple c = go c
      where
        incrementIfLess highest h@(v, c)
          | v < highest = v + c
          | otherwise = v
        go a
          | isFinished = head a
          | otherwise = go (zipWith (curry (incrementIfLess largest)) a c)
          where
            isFinished = all (== head a) (tail a)
            largest = foldl max 0 a

carmichaelsTotient :: (Integral a) => a -> a
carmichaelsTotient a = carmichaelsTotientGivenFactorisation (primeFactorisation a)

carmichaelsTotientOfSemiprimeGivenFactors :: (Integral a) => a -> a -> a
carmichaelsTotientOfSemiprimeGivenFactors p q = (a `div` g) * b
  where
    a = p - 1
    b = q - 1
    g = gcd a b
