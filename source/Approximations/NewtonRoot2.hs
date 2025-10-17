module Approximations.NewtonRoot2 where

import Approximations (formattedDigitsOf)
import Data.Bits (Bits (shiftL, shiftR))
import GHC.Real (Ratio ((:%)))

newtonRationals :: [Rational]
newtonRationals = map (\(a, b, _) -> a :% b) (iterate nextNewton (3, 2, 8))
  where
    nextNewton (p, q, n) = (p', q', n')
      where
        n' = ((n `shiftR` 1) * (n + 1)) `shiftL` 3
        p' = (n `shiftL` 1) + 1
        q' = (p * q) `shiftL` 1

main :: IO ()
main = do
  putStrLn (formattedDigitsOf newtonRationals)
