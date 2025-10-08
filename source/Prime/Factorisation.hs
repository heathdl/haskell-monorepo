module Prime.Factorisation (primeFactorisation, primeFactorMultiset, primeDivisors) where

import Prime (primes)

primeFactorisation :: (Integral a, Ord a) => a -> [(a, Int)]
primeFactorisation value
  | value < 2 = []
  | value < 3 = [(value, 1)]
  | otherwise = go value primes
  where
    go 1 _ = []
    go value (divisor : divisors)
      | divisor * divisor > value = [(value, 1)]
      | value `mod` divisor == 0 = (divisor, termCount) : go remainder divisors
      | otherwise = go value divisors
      where
        (termCount, remainder) = divide divisor value 0
    divide divisor value count
      | remainder == 0 = divide divisor quotient (count + 1)
      | otherwise = (count, value)
      where
        (quotient, remainder) = value `quotRem` divisor

primeFactorMultiset :: (Integral a) => a -> [a]
primeFactorMultiset value = concatMap (uncurry (flip replicate)) (primeFactorisation value)

primeDivisors :: (Integral a) => a -> [a]
primeDivisors value = map fst (primeFactorisation value)

main :: IO ()
main = do
  print (primeFactorisation (2 * 3 * 3 * 3 * 5 * 5 * 11))
