module Sequence.Mersenne (lucasLehmer, mersenneNumbers, mersennePrimePairs, mersennePrimes, mersenneExponents) where

import Prime (isPrime)

lucasLehmer :: (Integral a) => a -> a -> Bool
lucasLehmer 2 _ = True
lucasLehmer n m = go (n - 2) 4
  where
    go 0 s = s `mod` m == 0
    go k s = go (k - 1) ((s * s - 2) `mod` m)

mersenneNumbers :: (Integral a) => [(a, a)]
mersenneNumbers = iterate (\(x, e) -> (x * 2 + 1, e + 1)) (3, 2)

mersennePrimePairs :: (Integral a) => [(a, a)]
mersennePrimePairs = filter (uncurry (flip lucasLehmer)) mersenneNumbers

mersennePrimes :: (Integral a) => [a]
mersennePrimes = map fst mersennePrimePairs

mersenneExponents :: (Integral a) => [a]
mersenneExponents = map snd mersennePrimePairs

main :: IO ()
main = do
  print (take 15 mersenneNumbers)
  mapM_ print (mersennePrimes :: [Integer])