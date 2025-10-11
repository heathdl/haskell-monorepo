module Sequence.Mersenne (lucasLehmer, mersenneNumbers, mersennePrimePairs, mersennePrimes, mersenneExponents) where

import Prime (isPrime, primes)

lucasLehmer :: (Integral a) => a -> a -> Bool
lucasLehmer 2 _ = True
lucasLehmer n m = go (n - 2) 4
  where
    go 0 s = s `mod` m == 0
    go k s = go (k - 1) ((s * s - 2) `mod` m)

mersenneNumbers :: (Integral a) => [(a, a)]
mersenneNumbers = iterate (\(x, e) -> (x + x + 1, e + 1)) (3, 2)

-- https://oeis.org/A001348
mersennePrimeCandidates :: (Integral a) => [(a, a)]
mersennePrimeCandidates = go mersenneNumbers primes
  where
    go ms@(m@(_, e) : ms') ps@(p : ps')
      | e == p = m : go ms' ps'
      | e > p = go ms ps'
      | otherwise = go ms' ps

mersennePrimePairs :: (Integral a) => [(a, a)]
mersennePrimePairs = filter (uncurry (flip lucasLehmer)) mersennePrimeCandidates

-- https://oeis.org/A000668
mersennePrimes :: (Integral a) => [a]
mersennePrimes = map fst mersennePrimePairs

-- https://oeis.org/A000043
mersenneExponents :: (Integral a) => [a]
mersenneExponents = map snd mersennePrimePairs

main :: IO ()
main = do
  mapM_ print (mersennePrimes :: [Integer])
