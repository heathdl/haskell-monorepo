module Approximations.PellRoot2 (pellRationals) where

import Approximations (formattedDigitsOf)
import Data.Ratio ((%))

pellIteration :: (Integral a) => (a, a) -> (a, a)
pellIteration (x, y) = (x + 2 * y, x + y)

pellRationals :: Integer -> [Rational]
pellRationals 1 = map (uncurry (%)) (iterate pellIteration (1, 1))
pellRationals digitsPerOutput = map (uncurry (%)) (iterate (iterateN k pellIteration) (1, 1))
  where
    k = ceiling (fromInteger digitsPerOutput / growth)
    growth = 2 * logBase 10 (1.0 + sqrt 2)

    iterateN 0 _ p = p
    iterateN n f p = iterateN (n - 1) f (f p)

main :: IO ()
main = do
  putStrLn (formattedDigitsOf (pellRationals 1024))
