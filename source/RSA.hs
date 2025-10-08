module RSA where

import Data.Char (chr, ord)
import Prime.Tests.MillerRabin (millerRabinNextPrimeHigherThan)
import Prime.Totients (carmichaelsTotientOfPrimes)
import RaiseToPowerModulo (raiseToSomePowerModulo)
import System.Random (Random (randomR), RandomGen, StdGen, mkStdGen, split)

generatePrimePair :: (RandomGen g, Integral a, Random a) => Int -> (a, a) -> g -> ((a, a), g)
generatePrimePair rounds range gen = ((fromInteger p, fromInteger q), genQ)
  where
    (lowerP, genP) = randomR range gen
    (p, genP') = millerRabinNextPrimeHigherThan rounds (toInteger lowerP) genP
    (lowerQ, genQ') = randomR range genP'
    (q, genQ) = millerRabinNextPrimeHigherThan rounds (toInteger lowerQ) genQ'

type RSAParameters a = (a, a, a) -- (modulus, publicExponent, privateExponent)

rsaEncrypt :: (Integral a) => RSAParameters a -> a -> a
rsaEncrypt (n, e, _) m = raiseToSomePowerModulo m e n

rsaDecrypt :: (Integral a) => RSAParameters a -> a -> a
rsaDecrypt (n, _, d) c = raiseToSomePowerModulo c d n

rsaEncryptString :: (Integral a) => RSAParameters a -> String -> [a]
rsaEncryptString params = map (rsaEncrypt params . fromIntegral . ord)

rsaDecryptString :: (Integral a) => RSAParameters a -> [a] -> String
rsaDecryptString params = map (chr . fromIntegral . rsaDecrypt params)

generateRSAParameters :: (RandomGen g, Random a, Integral a) => Int -> (a, a) -> g -> (RSAParameters a, g)
generateRSAParameters rounds range gen = ((n, e, d'), gen')
  where
    (primePair, gen') = generatePrimePair rounds range gen
    n = uncurry (*) primePair
    λn = uncurry carmichaelsTotientOfPrimes primePair
    e = 2 ^ 16 + 1

    (_, d, _) = extendedEuclid e λn
    d' = d `mod` λn

    extendedEuclid a 0 = (a, 1, 0)
    extendedEuclid a b = (g, x, y)
      where
        (g, x1, y1) = extendedEuclid b (a `mod` b)
        x = y1
        y = x1 - (a `div` b) * y1

main :: IO ()
main = do
  putStrLn ("Encrypted: " ++ show c1)
  putStrLn ("Decrypted: " ++ show m1')

  putStrLn ("Encrypted String: " ++ show c2)
  putStrLn ("Decrypted String: " ++ show m2')
  where
    (rsaState, _) = generateRSAParameters 5 range generator

    m1 = 12345678 :: Integer
    c1 = rsaEncrypt rsaState m1
    m1' = rsaDecrypt rsaState c1

    m2 = "3.141592653589793238462643383279502884"
    c2 = rsaEncryptString rsaState m2
    m2' = rsaDecryptString rsaState c2

    exponent = 2 ^ 5
    range = (2 ^ exponent, 2 ^ exponent * 2 - 1) :: (Integer, Integer)
    generator = mkStdGen 1
