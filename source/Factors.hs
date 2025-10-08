module Factors (primeFactorisationSubsets, countFactors, factorisationOfFactors, calculateFactors, nonPrimeFactorisationsFromFactorisation, nonPrimeFactorisations) where

import Multiset (Multiset, canonicalisePartitions, computePartitions, fromSortedList, multiSetDoubleMap)
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

calculateFactors :: (Integral a) => a -> [a]
calculateFactors x = map product (factorisationOfFactors x)

nonPrimeFactorisationsFromFactorisation :: (Integral a) => [(a, Int)] -> [Multiset a]
nonPrimeFactorisationsFromFactorisation factorisation = map Multiset.fromSortedList products
  where
    products = map (map (multiSetDoubleMap (^) product)) partitions
    partitions = canonicalisePartitions (computePartitions factorisation)

nonPrimeFactorisations :: (Integral a) => a -> [Multiset a]
nonPrimeFactorisations x = nonPrimeFactorisationsFromFactorisation (primeFactorisation x)

main :: IO ()
main = do
  print (calculateFactors 360)
  print (factorisationOfFactors 360)
  print (countFactors (primeFactorisation 360))
