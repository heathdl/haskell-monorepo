module Factors (primeFactorisationSubsets, factorisationOfFactors, factors, main) where

import Prime.Factorisation (primeFactorisation)
import Prime.Factorisation.Pairwise (primeFactorPairs)
import Sort.MergeSort (mergeAll)

primeFactorisationSubsets :: (Integral a) => [(a, Int)] -> [[a]]
primeFactorisationSubsets [] = [[]]
primeFactorisationSubsets ((p, c) : xs) = mergeAll compareFactorisations (map prefixes subsequent)
  where
    subsequent = primeFactorisationSubsets xs
    compareFactorisations a b = product a < product b
    prefixes s = [replicate a p ++ s | a <- [0 .. c]]

countFactors :: (Integral a) => [(a, Int)] -> Int
countFactors [] = 1
countFactors ((_, count) : xs) = (count + 1) * countFactors xs

factorisationOfFactors :: (Integral a) => a -> [[a]]
factorisationOfFactors x = primeFactorisationSubsets (primeFactorisation x)

factors :: (Integral a) => a -> [a]
factors x = map product (factorisationOfFactors x)

highlyComposite :: (Integral a) => [a]
highlyComposite = nextHighlyComposite primeFactorPairs 0
  where
    nextHighlyComposite ((x, factors) : xs) currentHighest
      | factorCount > currentHighest = x : nextHighlyComposite xs factorCount
      | otherwise = nextHighlyComposite xs currentHighest
      where
        factorCount = countFactors factors

main :: IO ()
main = do
  print (factors 360)
  print (factorisationOfFactors 360)
  print (countFactors (primeFactorisation 360))
  print (take 25 highlyComposite)