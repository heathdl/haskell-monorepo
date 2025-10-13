module Prime.Coprimes (areFactorisationsCoprime, areCoprime, calculateCoprimes, compositeCoprimes) where

import Multiset (Multiset)
import Prime.Factorisation (primeFactorisation)
import Prime.Factorisation.Pairwise (primeFactorPairs)

areFactorisationsCoprime :: (Integral a) => Multiset a -> Multiset a -> Bool
areFactorisationsCoprime [] _ = True
areFactorisationsCoprime _ [] = True
areFactorisationsCoprime a@((ah, _) : as) b@((bh, _) : bs)
  | ah < bh = areFactorisationsCoprime as b
  | ah > bh = areFactorisationsCoprime a bs
  | otherwise = False

areCoprime :: (Integral a) => a -> a -> Bool
areCoprime a 0 = a == 1 || a == -1
areCoprime a b = areCoprime b (a `mod` b)

calculateCoprimes :: (Integral a) => a -> [a]
calculateCoprimes x = map fst (filter (areFactorisationsCoprime factorisation . snd) candidates)
  where
    factorisation = primeFactorisation x
    candidates = takeWhile (\a -> x > fst a) primeFactorPairs

compositeCoprimes :: (Integral a) => a -> [a]
compositeCoprimes x = map fst (filter isCompositeCoprime candidates)
  where
    factorisation = primeFactorisation x
    candidates = takeWhile (\(a, _) -> a < x) primeFactorPairs
    isCompositeCoprime (_, b) = length b > 1 && areFactorisationsCoprime factorisation b

main :: IO ()
main = do
  print (calculateCoprimes 35776)
