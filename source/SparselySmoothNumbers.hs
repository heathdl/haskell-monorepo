module SparselySmoothNumbers where

import Data.Bifunctor (Bifunctor (bimap))
import Prime (primes)

sparselySmoothNumbers :: (Integral a) => [a] -> [a]
sparselySmoothNumbers primes = map fst (sparselySmoothFactorisationPairs primes)

sparselySmoothFactorisationPairs :: (Integral a) => [a] -> [(a, [a])]
sparselySmoothFactorisationPairs primes = composites
  where
    composites = (1, []) : mergeAll [map (bimap (p *) (p :)) composites | p <- primes]

    merge [] y = y
    merge x [] = x
    merge xs@(x@(xk, xv) : xs') ys@(y@(yk, yv) : ys')
      | xk < yk = x : merge xs' ys
      | xk > yk = y : merge xs ys'
      | otherwise = x : merge xs' ys'

    mergeAll :: (Ord a) => [[(a, b)]] -> [(a, b)]
    mergeAll = foldr merge []

main :: IO ()
main =
  do
    print (take 50 (sparselySmoothFactorisationPairs (takeWhile (<= 5) primes)))
