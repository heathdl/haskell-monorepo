module RaiseToPowerModulo where

import Prime (primes)

eulersTotient :: Int -> Int
eulersTotient value
  | value < 1 = 0
  | value < 3 = 1
  | otherwise = go value primes
  where
    go :: Int -> [Int] -> Int
    go 1 _ = 1
    go value (divisor : divisors)
      | divisor * divisor > value = value - 1
      | value `mod` divisor == 0 = (divisor - 1) * (divisor ^ (termCount - 1)) * go remainder divisors
      | otherwise = go value divisors
      where
        (termCount, remainder) = divide divisor value 0
    divide divisor value count
      | remainder == 0 = divide divisor quotient (count + 1)
      | otherwise = (count, value)
      where
        (quotient, remainder) = value `quotRem` divisor

raiseToSomePowerModulo :: Int -> Int -> Int -> Int
raiseToSomePowerModulo _ 0 _ = 1
raiseToSomePowerModulo base value n = go (base `mod` n) value
  where
    go base value
      | remainder == 1 = (base * subsequent) `mod` n
      | otherwise = subsequent
      where
        (quotient, remainder) = value `quotRem` 2
        subsequent = raiseToSomePowerModulo (base * base `mod` n) quotient n

main :: IO ()
main = do
  print (raiseToSomePowerModulo a m n)
  where
    a = 71273612313
    m = 91137987423 `mod` eulersTotient n
    n = 1000