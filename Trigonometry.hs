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

scθ :: (RealFloat a) => [(a, a)]
scθ = iterate next (1, 0)
  where
    next (sin, cos) = (sin', cos')
      where
        sin' = sin / sqrt (2 * (1 + cos))
        cos' = sqrt ((1 + cos) / 2)

sinCos :: (RealFloat a) => a -> (a, a)
sinCos value
  | even integral = (sinValue, cosValue)
  | otherwise = (-sinValue, -cosValue)
  where
    (integral, fractional, bitLength) = extractFloatComponents value
    (sinValue, cosValue) = go fractional bitLength scθ

    go :: (RealFloat a) => Integer -> Int -> [(a, a)] -> (a, a)
    go 0 _ _ = (0.0, 1.0)
    go _ 0 _ = (0.0, 1.0)
    go fractional bitLength ((sθ, cθ) : scθs)
      | testBit fractional (bitLength - 1) = (sinΣ', cosΣ')
      | otherwise = (sinΣ, cosΣ)
      where
        sinΣ' = sinΣ * cθ + cosΣ * sθ
        cosΣ' = cosΣ * cθ - sinΣ * sθ
        (sinΣ, cosΣ) = go fractional (bitLength - 1) scθs

sin :: (RealFloat a) => a -> a
sin value = fst (sinCos value)

cos :: (RealFloat a) => a -> a
cos value = snd (sinCos value)

tan :: (RealFloat a) => a -> a
tan value = uncurry (/) (sinCos value)

main :: IO ()
main = do
  let values = [1/4, 1/3, 3603 / 2048]
  mapM_ (\x -> do
    print x
    putStrLn ("sin " ++ show (Trigonometry.sin x))
    putStrLn ("cos " ++ show (Trigonometry.cos x))
    putStrLn ("tan " ++ show (Trigonometry.tan x))
    putStr "\n"
    ) values