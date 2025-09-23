module Prime.EulersTotient (eulersTotient, eulersTotientGivenFactorisation) where

import Prime.Factorisation (primeFactorisation)

eulersTotientGivenFactorisation :: (Integral a) => [(a, Int)] -> a
eulersTotientGivenFactorisation factorisation = product (map (\(factor, count) -> (factor - 1) * factor ^ (count - 1)) factorisation)

eulersTotient :: (Integral a) => a -> a
eulersTotient = eulersTotientGivenFactorisation . primeFactorisation 
