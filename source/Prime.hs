module Prime (primes, isPrime) where

import Data.Bool (Bool)
import Prime.Factorisation.Pairwise (primeFactorPairs)
import Prime.Tests.MillerRabin (deterministicMillerRabinPrimalityTest)
import Prime.Tests.TrailDivision (internalTrialDivision)
import System.Random (Random, mkStdGen)

isPrimeTrialDivision :: (Integral a) => a -> Bool
isPrimeTrialDivision = internalTrialDivision primes

primes :: (Integral a) => [a]
primes = [x | (x, [(_, 1)]) <- tail primeFactorPairs]

isPrime :: (Integral a) => a -> Bool
isPrime value
  | value <= 2 ^ 10 = isPrimeTrialDivision value
  | value <= 2 ^ 64 = deterministicMillerRabinPrimalityTest value
  | otherwise = error "primes list only supports deterministic checks"

main :: IO ()
main = do
  print (take 100 primes :: [Int])
  print (isPrime value)
  where
    value = (3 * 3 * 5 * 11) :: Int
