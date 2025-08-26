module MillerRabin
  ( millerRabinDecompose,
    millerRabinWitnessGivenDecomposition,
    millerRabinWitness,
    millerRabinPrimalityTest,
  )
where

import RaiseToPowerModulo (raiseToSomePowerModulo)
import System.Random (randomRIO)

millerRabinDecompose :: (Integral a) => a -> (Int, a)
millerRabinDecompose value = go (value - 1) 0
  where
    go excess leadingZeros
      | even excess = go (excess `div` 2) (leadingZeros + 1)
      | otherwise = (leadingZeros, excess)

millerRabinWitnessGivenDecomposition :: (Integral a) => a -> (Int, a) -> a -> Bool
millerRabinWitnessGivenDecomposition value (s, d) base
  | value < 2 = False
  | value == 2 = True
  | even value = False
  | otherwise = checkWitness (raiseToSomePowerModulo base d value) s
  where
    x = raiseToSomePowerModulo base d value

    checkWitness x 0 = x == 1
    checkWitness x k
      | y == 1 && x /= 1 && x /= value - 1 = False
      | otherwise = checkWitness y (k - 1)
      where
        y = (x * x) `mod` value

millerRabinWitness :: (Integral a) => a -> a -> Bool
millerRabinWitness value =
  millerRabinWitnessGivenDecomposition
    value
    (millerRabinDecompose value)

-- http://miller-rabin.appspot.com/
millerRabinBases :: (Integral a) => [a]
millerRabinBases = [2, 325, 9375, 28178, 450775, 9780504, 1795265022]

-- These are values that are not prime divisors of at least one of the bases, it is
-- hardcoded as calculating them on startup was slow due to factorisation.
millerRabinExceptions :: (Integral a) => [a]
millerRabinExceptions = [2, 3, 5, 13, 19, 73, 193, 407521, 299210837]

deterministicMillerRabinPrimalityTest :: (Integral a) => a -> Bool
deterministicMillerRabinPrimalityTest value
  | value > 2 ^ 64 = error "input exceeds 2^64, cannot confirm reliablity of 'deterministic' Miller-Rabin test"
  | value `elem` millerRabinExceptions = True
  | otherwise = all (millerRabinWitnessGivenDecomposition value decomposition) millerRabinBases
  where
    decomposition = millerRabinDecompose value

millerRabinPrimalityTest :: (Integral a) => a -> a -> IO Bool
millerRabinPrimalityTest 0 _ = return True
millerRabinPrimalityTest rounds value =
  do
    a <- fmap fromInteger (randomRIO (2, toInteger value - 2))
    if millerRabinWitnessGivenDecomposition value decomposition a
      then millerRabinPrimalityTest (rounds - 1) value
      else return False
  where
    decomposition = millerRabinDecompose value

main :: IO ()
main = do
  isPrime <- millerRabinPrimalityTest 64 312
  print isPrime
  isPrime <- millerRabinPrimalityTest 64 97
  print isPrime

  print (deterministicMillerRabinPrimalityTest 312)
  print (deterministicMillerRabinPrimalityTest 97)

  isPrime <- millerRabinPrimalityTest 128 (36683066477630951724340087046591610564446921147353414096088600256138856905183639589419929766697136525547236850070079729469226627 :: Integer)
  print isPrime
