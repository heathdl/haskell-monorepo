module Trigonometry (sinCos, Trigonometry.sin, Trigonometry.cos, Trigonometry.tan) where

import Data.Bits (testBit)
import Data.Fixed (mod')

normalise :: (Integral a) => a -> (a, Int)
normalise 0 = (0, 0)
normalise value = go value 0
  where
    go value depth
      | even value = go (value `div` 2) (depth + 1)
      | otherwise = (value, depth)

extractFloatComponents :: (RealFloat a) => a -> (Int, Integer, Int)
extractFloatComponents x
  | x == 0 = (0, 0, 0)
  | otherwise = (integral, adjustedMantissa, adjustedExponent)
  where
    integral = floor x
    fractional = abs (x `mod'` 1)

    (mantissa, exponent) = decodeFloat fractional
    (adjustedMantissa, offset) = normalise mantissa
    adjustedExponent = -exponent - offset

csθ :: (RealFloat a) => [(a, a)]
csθ = iterate next (0, 1)
  where
    next (cos, _) = (cos', sin')
      where
        cos' = sqrt ((1 + cos) / 2)
        sin' = sqrt (1 - cos' ^ 2)

sinCos :: (RealFloat a) => a -> (a, a)
sinCos value
  | even integral = (cosValue, sinValue)
  | otherwise = (-cosValue, -sinValue)
  where
    (integral, fractional, bitLength) = extractFloatComponents value
    (cosValue, sinValue) = go fractional bitLength csθ

    go :: (RealFloat a) => Integer -> Int -> [(a, a)] -> (a, a)
    go 0 _ _ = (1.0, 0.0)
    go _ 0 _ = (1.0, 0.0)
    go fractional bitLength ((cθ, sθ) : csθs)
      | testBit fractional (bitLength - 1) = (cosΣ', sinΣ')
      | otherwise = (cosΣ, sinΣ)
      where
        cosΣ' = cosΣ * cθ - sinΣ * sθ
        sinΣ' = sinΣ * cθ + cosΣ * sθ
        (cosΣ, sinΣ) = go fractional (bitLength - 1) csθs

cos :: (RealFloat a) => a -> a
cos value = fst (sinCos value)

sin :: (RealFloat a) => a -> a
sin value = snd (sinCos value)

tan :: (RealFloat a) => a -> a
tan value = sinValue / cosValue
  where
    (cosValue, sinValue) = sinCos value

main :: IO ()
main = do
  print (Trigonometry.sin value)
  print (Trigonometry.cos value)
  print (Trigonometry.tan value)
  where
    value = 1 / 3 :: Double
    range = 256
    resolution = 32