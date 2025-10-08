module Prime.Tests.TrailDivision (internalTrialDivision, naiveTrialDivision) where

internalTrialDivision :: (Integral a) => [a] -> a -> Bool
internalTrialDivision primes 1 = False
internalTrialDivision primes 2 = True
internalTrialDivision primes 3 = True
internalTrialDivision primes value = all (failsToDivide value) (takeWhile (isLessThanSquareRootOf value) primes)
  where
    failsToDivide x y = x `mod` y /= 0
    isLessThanSquareRootOf x y = y * y <= x

naiveTrialDivision :: (Integral a) => a -> Bool
naiveTrialDivision 1 = False
naiveTrialDivision 2 = True
naiveTrialDivision 3 = True
naiveTrialDivision value
  | even value = False
  | value `mod` 3 == 0 = False
  | otherwise = go 5 2
  where
    go check increment
      | check * check > value = True
      | value `mod` check == 0 = False
      | otherwise = go (check + increment) (6 - increment)
