module MillerRabin (millerRabinWitness, millerRabinPrimalityTest) where

import Data.Bits (Bits (shiftR), FiniteBits (countTrailingZeros))
import RaiseToPowerModulo (raiseToSomePowerModulo)
import System.Random (randomRIO)

millerRabinWitness :: Int -> Int -> Bool
millerRabinWitness value base
  | value < 2 = False
  | value == 2 = True
  | even value = False
  | otherwise = checkWitness x s
  where
    s = countTrailingZeros (value - 1)
    d = (value - 1) `shiftR` s
    x = raiseToSomePowerModulo base d value

    checkWitness x 0 = x == 1
    checkWitness x k
      | y == 1 && x /= 1 && x /= value - 1 = False
      | otherwise = checkWitness y (k - 1)
      where
        y = (x * x) `mod` value

-- http://miller-rabin.appspot.com/
millerRabinBases :: [Int]
millerRabinBases = [2, 325, 9375, 28178, 450775, 9780504, 1795265022]

-- Can be calulcated through this, but it is slow due to the factorisation.
-- millerRabinFailures = map head (group (sort (concatMap primeDivisors [2, 325, 9375, 28178, 450775, 9780504, 1795265022])))
millerRabinFailures :: [Int]
millerRabinFailures = [2, 3, 5, 13, 19, 73, 193, 407521, 299210837]

deterministicMillerRabinPrimalityTest :: Int -> Bool
deterministicMillerRabinPrimalityTest value
  | value `elem` millerRabinFailures = True
  | otherwise = all (millerRabinWitness value) millerRabinBases

millerRabinPrimalityTest :: Int -> Int -> IO Bool
millerRabinPrimalityTest 0 _ = return True
millerRabinPrimalityTest rounds value =
  do
    a <- randomRIO (2, value - 2)
    if millerRabinWitness value a
      then millerRabinPrimalityTest (rounds - 1) value
      else return False

main :: IO ()
main = do
  isPrime <- millerRabinPrimalityTest 64 312
  print isPrime
  isPrime <- millerRabinPrimalityTest 64 97
  print isPrime