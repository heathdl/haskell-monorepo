module Prime.Factorisation.Pairwise (primeFactorMultisetPairs, primeFactorPairs) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Multiset (Multiset)

createPrimeList :: (Integral a) => (Map a [b] -> a -> Maybe [a] -> [b]) -> [(a, [b])]
createPrimeList formatFactors = (1, []) : go 2 Map.empty (Map.singleton 1 [])
  where
    go n !compositeMap !factorMap =
      (n, factors) : go (n + 1) compositeMap' factorMap'
      where
        (factors, compositeMap') =
          maybe
            (handlePrime compositeMap factorMap n)
            (handleComposite compositeMap factorMap n)
            (Map.lookup n compositeMap)
        factorMap' = Map.insert n factors factorMap

    handlePrime compositeMap factorMap p = (factors, compositeMap')
      where
        factors = formatFactors factorMap p Nothing
        compositeMap' = Map.insertWith (++) (p * p) [p] compositeMap

    handleComposite compositeMap factorMap n ps@(p : _) = (factors, compositeMap')
      where
        factors = formatFactors factorMap n (Just ps)
        compositeMap' =
          foldl'
            (\mp pr -> Map.insertWith (++) (n + pr) [pr] mp)
            (Map.delete n compositeMap)
            ps

primeFactorPairs :: (Integral a) => [(a, Multiset a)]
primeFactorPairs = createPrimeList formatFactors
  where
    formatFactors factorMap n Nothing = [(n, 1)]
    formatFactors factorMap n (Just ps@(p : _)) = factors
      where
        knownFactors = factorMap Map.! (n `div` p)
        factors = case knownFactors of
          ((q, count) : rest) | p == q -> (q, count + 1) : rest
          _ -> (p, 1) : knownFactors

primeFactorMultisetPairs :: (Integral a) => [(a, [a])]
primeFactorMultisetPairs = createPrimeList formatFactors
  where
    formatFactors factorMap n Nothing = [n]
    formatFactors factorMap n (Just ps@(p : _)) = p : factorMap Map.! (n `div` p)

main :: IO ()
main = do
  print (take 100 (map (product . snd) primeFactorMultisetPairs))
  print (take 100 primeFactorPairs)