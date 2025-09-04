module Prime.EulersTotient (eulersTotient) where

import Prime.Factorisation (primeFactorisation)

eulersTotient :: (Integral a) => a -> a
eulersTotient value
  | value < 1 = 0
  | otherwise = product [(factor - 1) * factor ^ (count - 1) | (factor, count) <- primeFactorisation value]
